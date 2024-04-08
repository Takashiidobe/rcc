use std::collections::HashMap;

use crate::{ErrorReporting, Keyword, Punct, SourceLocation, Token, TokenKind, P};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Ptr(P<Type>),
    Int,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node<Kind> {
    pub kind: Kind,
    pub loc: SourceLocation,
    pub r#type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BindingKind {
    GlobalVar { init_data: Option<Vec<u8>> },
    LocalVar { stack_offset: i64 },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub kind: BindingKind,
    pub name: Vec<u8>,
    pub r#type: Type,
    pub loc: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(i64),
    Add(P<ExprNode>, P<ExprNode>),
    Sub(P<ExprNode>, P<ExprNode>),
    Mul(P<ExprNode>, P<ExprNode>),
    Div(P<ExprNode>, P<ExprNode>),

    Neg(P<ExprNode>),

    Lt(P<ExprNode>, P<ExprNode>),
    Lte(P<ExprNode>, P<ExprNode>),
    Eq(P<ExprNode>, P<ExprNode>),
    Ne(P<ExprNode>, P<ExprNode>),

    Assign(P<ExprNode>, P<ExprNode>),
    Var(Binding),

    Addr(P<ExprNode>),
    Deref(P<ExprNode>),
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(ExprNode),
    Return(ExprNode),
    Block(Vec<StmtNode>),
    If(P<ExprNode>, P<StmtNode>, Option<P<StmtNode>>),
    For(
        Option<P<StmtNode>>,
        Option<P<ExprNode>>,
        Option<P<ExprNode>>,
        P<StmtNode>,
    ),
}

pub type ExprNode = Node<ExprKind>;
pub type StmtNode = Node<StmtKind>;

#[derive(Default)]
pub struct Parser {
    pub source: Vec<u8>,
    pub tokens: Vec<Token>,
    pub variables: HashMap<String, i64>,
    pub index: usize,
    pub stack_offset: i64,
    pub stack_size: usize,
}

impl ErrorReporting for Parser {
    fn src(&self) -> Vec<u8> {
        self.source.clone()
    }
}

impl Parser {
    pub fn new(source: Vec<u8>, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            ..Default::default()
        }
    }

    // parse = stmt*
    pub fn parse(&mut self) -> (Vec<StmtNode>, usize) {
        self.skip("{");
        let stmts = vec![self.compound_stmt()];
        self.ensure_done();
        (stmts, self.stack_size)
    }

    // compound_stmt = "{" (declaration | stmt)* "}
    fn compound_stmt(&mut self) -> StmtNode {
        let mut stmts = vec![];
        let loc = self.loc();
        while self.peek().kind != TokenKind::Punct(Punct::RightBrace) {
            if self.r#match("int") {
                self.declaration(&mut stmts);
            } else {
                stmts.push(self.stmt());
            }
        }
        self.skip("}");
        StmtNode {
            kind: StmtKind::Block(stmts),
            loc,
            r#type: Type::Int,
        }
    }

    // declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self, stmts: &mut Vec<StmtNode>) {
        let base_type = self.declspec();

        let mut count = 0;
        while self.peek().kind != TokenKind::Punct(Punct::Semicolon) {
            if count > 0 {
                self.skip(",");
            }
            count += 1;

            let loc = self.loc();
            let (r#type, val) = self.declarator(&base_type);

            let stack_offset = if self.variables.contains_key(&val) {
                *self.variables.get(&val).unwrap()
            } else {
                let stack_offset = self.stack_offset();
                self.variables.insert(val.clone(), stack_offset);
                stack_offset
            };
            let var_data = Binding {
                kind: BindingKind::LocalVar { stack_offset },
                name: val.bytes().collect(),
                r#type: Type::Int,
                loc,
            };

            if self.peek().kind != TokenKind::Punct(Punct::Eq) {
                continue;
            }

            self.advance();
            let lhs = ExprNode {
                kind: ExprKind::Var(var_data),
                loc,
                r#type,
            };
            let rhs = self.assign();
            let rhs_type = rhs.r#type.clone();

            stmts.push(StmtNode {
                kind: StmtKind::Expr(ExprNode {
                    kind: ExprKind::Assign(P::new(lhs), P::new(rhs)),
                    loc,
                    r#type: rhs_type,
                }),
                loc,
                r#type: Type::Unit,
            });
        }
    }

    // declspec = "int"
    fn declspec(&mut self) -> Type {
        self.skip("int");
        Type::Int
    }

    // declarator = "*"* ident
    fn declarator(&mut self, base_type: &Type) -> (Type, String) {
        let mut r#type = base_type.clone();
        while let TokenKind::Punct(Punct::Star) = self.peek().kind {
            self.advance();
            r#type = Type::Ptr(P::new(r#type));
        }

        match self.peek().kind.clone() {
            TokenKind::Var(name) => {
                self.advance();
                (r#type, name)
            }
            _ => self.error_tok(self.peek(), "expected a variable name"),
        }
    }

    // expr-stmt = expr? ";"
    fn expr_stmt(&mut self) -> StmtNode {
        let loc = self.loc();
        if let TokenKind::Punct(Punct::Semicolon) = self.peek().kind {
            self.advance();
            return StmtNode {
                kind: StmtKind::Block(vec![]),
                loc,
                r#type: Type::Int,
            };
        }

        let node = self.expr();
        self.skip(";");
        StmtNode {
            kind: StmtKind::Expr(node.clone()),
            loc: node.loc,
            r#type: node.r#type,
        }
    }

    // stmt = "return" expr ";
    //      | "if" "(" expr ")" stmt ("else" stmt)?
    //      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
    //      | "{" compound-stmt
    //      | expr-stmt
    fn stmt(&mut self) -> StmtNode {
        let r#type = Type::Unit;
        if let TokenKind::Keyword(Keyword::Return) = self.peek().kind {
            self.advance();
            let node = self.expr();
            self.skip(";");
            return StmtNode {
                kind: StmtKind::Return(node.clone()),
                loc: node.loc,
                r#type,
            };
        }
        if let TokenKind::Keyword(Keyword::If) = self.peek().kind {
            let loc = self.loc();
            self.advance();
            self.skip("(");
            let cond = P::new(self.expr());
            self.skip(")");
            let then = P::new(self.stmt());
            let mut r#else = None;
            if let TokenKind::Keyword(Keyword::Else) = self.peek().kind {
                self.advance();
                r#else = Some(P::new(self.stmt()));
            }
            return StmtNode {
                kind: StmtKind::If(cond, then, r#else),
                loc,
                r#type,
            };
        }
        if let TokenKind::Keyword(Keyword::For) = self.peek().kind {
            let loc = self.loc();
            self.advance();
            self.skip("(");
            let init = Some(P::new(self.expr_stmt()));
            let cond = if self.peek().kind != TokenKind::Punct(Punct::Semicolon) {
                Some(P::new(self.expr()))
            } else {
                None
            };
            self.skip(";");
            let incr = if self.peek().kind != TokenKind::Punct(Punct::RightParen) {
                Some(P::new(self.expr()))
            } else {
                None
            };
            self.skip(")");
            let then = P::new(self.stmt());
            return StmtNode {
                kind: StmtKind::For(init, cond, incr, then),
                loc,
                r#type,
            };
        }
        if let TokenKind::Keyword(Keyword::While) = self.peek().kind {
            let loc = self.loc();
            self.advance();
            self.skip("(");
            let cond = Some(P::new(self.expr()));
            self.skip(")");
            let then = P::new(self.stmt());
            return StmtNode {
                kind: StmtKind::For(None, cond, None, then),
                loc,
                r#type,
            };
        }
        if let TokenKind::Punct(Punct::LeftBrace) = self.peek().kind {
            self.advance();
            return self.compound_stmt();
        }
        self.expr_stmt()
    }

    // expr = assign
    fn expr(&mut self) -> ExprNode {
        self.assign()
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> ExprNode {
        let mut node = self.equality();
        if let TokenKind::Punct(Punct::Eq) = self.peek().kind {
            self.advance();

            node.kind = ExprKind::Assign(P::new(node.clone()), P::new(self.assign()));
        }
        node
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> ExprNode {
        let mut node = self.relational();

        while let TokenKind::Punct(punct @ (Punct::EqEq | Punct::Ne)) = self.peek().kind {
            let loc = self.loc();
            self.advance();
            let lhs = P::new(node);
            let rhs = P::new(self.relational());
            node = ExprNode {
                kind: if punct == Punct::EqEq {
                    ExprKind::Eq(lhs, rhs)
                } else {
                    ExprKind::Ne(lhs, rhs)
                },
                loc,
                r#type: Type::Int,
            }
        }

        node
    }

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> ExprNode {
        let mut node = self.add();

        while let TokenKind::Punct(punct @ (Punct::Gt | Punct::Gte | Punct::Lt | Punct::Lte)) =
            self.peek().kind
        {
            let loc = self.loc();
            self.advance();
            node = ExprNode {
                kind: if punct == Punct::Gt {
                    ExprKind::Lt(P::new(self.add()), P::new(node))
                } else if punct == Punct::Gte {
                    ExprKind::Lte(P::new(self.add()), P::new(node))
                } else if punct == Punct::Lt {
                    ExprKind::Lt(P::new(node), P::new(self.add()))
                } else {
                    ExprKind::Lte(P::new(node), P::new(self.add()))
                },
                loc,
                r#type: Type::Int,
            }
        }

        node
    }

    // add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> ExprNode {
        let mut node = self.mul();

        while let TokenKind::Punct(punct @ (Punct::Plus | Punct::Minus)) = self.peek().kind {
            self.advance();
            let lhs = P::new(node);
            let rhs = P::new(self.mul());
            if punct == Punct::Plus {
                node = self.add_overload(lhs, rhs);
            } else {
                node = self.sub_overload(lhs, rhs);
            }
        }

        node
    }

    // mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> ExprNode {
        let mut node = self.unary();
        let loc = self.loc();

        while let TokenKind::Punct(punct @ (Punct::Star | Punct::Slash)) = self.peek().kind {
            self.advance();
            let lhs = P::new(node);
            let rhs = P::new(self.unary());
            node = ExprNode {
                kind: if punct == Punct::Star {
                    ExprKind::Mul(lhs, rhs)
                } else {
                    ExprKind::Div(lhs, rhs)
                },
                loc,
                r#type: Type::Int,
            }
        }

        node
    }

    // unary = ("+" | "-" | "*" | "&") unary
    //       | primary
    fn unary(&mut self) -> ExprNode {
        let loc = self.loc();

        if let TokenKind::Punct(
            punct @ (Punct::Minus | Punct::Plus | Punct::Ampersand | Punct::Star),
        ) = self.peek().kind
        {
            self.advance();

            if punct == Punct::Plus {
                return self.unary();
            } else {
                let lhs = P::new(self.unary());
                let r#type = if punct == Punct::Ampersand {
                    Type::Ptr(P::new(lhs.r#type.clone()))
                } else if punct == Punct::Star {
                    if let Type::Ptr(ref base) = lhs.r#type {
                        *base.clone()
                    } else {
                        Type::Int
                    }
                } else {
                    Type::Int
                };

                return ExprNode {
                    kind: if punct == Punct::Minus {
                        ExprKind::Neg(lhs)
                    } else if punct == Punct::Ampersand {
                        ExprKind::Addr(lhs)
                    } else {
                        ExprKind::Deref(lhs)
                    },
                    loc,
                    r#type,
                };
            }
        }

        self.primary()
    }

    // primary = "(" expr ")" | ident | num
    fn primary(&mut self) -> ExprNode {
        let loc = self.loc();
        match self.peek().kind.clone() {
            TokenKind::Number(val) => {
                self.advance();
                return ExprNode {
                    kind: ExprKind::Number(val),
                    loc,
                    r#type: Type::Int,
                };
            }
            TokenKind::Var(val) => {
                self.advance();
                let stack_offset = if self.variables.contains_key(&val) {
                    *self.variables.get(&val).unwrap()
                } else {
                    let stack_offset = self.stack_offset();
                    self.variables.insert(val.clone(), stack_offset);
                    stack_offset
                };
                return ExprNode {
                    kind: ExprKind::Var(Binding {
                        kind: BindingKind::LocalVar { stack_offset },
                        name: val.bytes().collect(),
                        r#type: Type::Int,
                        loc,
                    }),
                    loc,
                    r#type: Type::Int,
                };
            }
            TokenKind::Punct(Punct::LeftParen) => {
                self.advance();
                let expr = self.expr();
                self.skip(")");
                return expr;
            }
            _ => {}
        }
        self.error_tok(self.peek(), "Expected expression");
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn advance(&mut self) {
        if self.index >= self.tokens.len() {
            panic!("Unexpected end of file");
        }
        self.index += 1;
    }

    fn r#match(&self, s: &str) -> bool {
        let tok = self.peek();
        self.source[tok.loc.offset..(tok.loc.offset + tok.length)].eq(s.as_bytes())
    }

    fn skip(&mut self, s: &str) {
        if !self.r#match(s) {
            self.error_tok(self.peek(), &format!("Expected {}", s));
        }
        self.advance();
    }

    fn is_done(&self) -> bool {
        self.index >= self.tokens.len() || self.tokens[self.index].kind == TokenKind::Eof
    }

    fn ensure_done(&self) {
        match self.peek().kind {
            TokenKind::Eof => {}
            _ => self.error_tok(self.peek(), "extra token"),
        }
    }

    fn stack_offset(&mut self) -> i64 {
        self.stack_offset += 1;
        self.stack_size += 8;
        self.align_stack(16);
        -self.stack_offset
    }

    fn align_stack(&mut self, align: usize) {
        self.stack_size = ((self.stack_offset * 8) as usize + align - 1) / align * align;
    }

    fn loc(&self) -> SourceLocation {
        self.peek().loc
    }

    fn add_overload(&self, lhs: P<ExprNode>, rhs: P<ExprNode>) -> ExprNode {
        let loc = self.loc();
        let mut lhs = lhs;
        let mut rhs = rhs;

        if let Type::Int = lhs.r#type {
            if let Type::Ptr(_) = rhs.r#type {
                std::mem::swap(&mut lhs, &mut rhs);
            }
        }

        match (&lhs.r#type, &rhs.r#type) {
            (Type::Int, Type::Int) => ExprNode {
                kind: ExprKind::Add(lhs, rhs),
                loc,
                r#type: Type::Int,
            },
            (Type::Ptr(_), Type::Int) => {
                let rhs = P::new(ExprNode {
                    kind: ExprKind::Mul(
                        P::new(ExprNode {
                            kind: ExprKind::Number(8),
                            loc,
                            r#type: Type::Int,
                        }),
                        rhs,
                    ),
                    loc,
                    r#type: Type::Int,
                });
                let r#type = lhs.r#type.clone();
                ExprNode {
                    kind: ExprKind::Add(lhs, rhs),
                    loc,
                    r#type,
                }
            }
            _ => self.error_at(loc.offset, "invalid operands"),
        }
    }

    fn sub_overload(&self, lhs: P<ExprNode>, rhs: P<ExprNode>) -> ExprNode {
        let loc = self.loc();
        match (&lhs.r#type, &rhs.r#type) {
            (Type::Int, Type::Int) => ExprNode {
                kind: ExprKind::Sub(lhs, rhs),
                loc,
                r#type: Type::Int,
            },
            (Type::Ptr(_), Type::Int) => {
                let rhs = P::new(ExprNode {
                    kind: ExprKind::Mul(synth_num(8, loc), rhs),
                    loc,
                    r#type: Type::Int,
                });
                let r#type = lhs.r#type.clone();
                ExprNode {
                    kind: ExprKind::Sub(lhs, rhs),
                    loc,
                    r#type,
                }
            }
            (Type::Ptr(_), Type::Ptr(_)) => {
                let node = P::new(ExprNode {
                    kind: ExprKind::Sub(lhs, rhs),
                    loc,
                    r#type: Type::Int,
                });
                ExprNode {
                    kind: ExprKind::Div(node, synth_num(8, loc)),
                    loc,
                    r#type: Type::Int,
                }
            }
            _ => self.error_at(loc.offset, "invalid operands"),
        }
    }
}

fn synth_num(v: i64, loc: SourceLocation) -> P<ExprNode> {
    P::new(ExprNode {
        kind: ExprKind::Number(v),
        loc,
        r#type: Type::Int,
    })
}
