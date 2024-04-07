use crate::{ErrorReporting, Punct, SourceLocation, Token, TokenKind, Type, P};

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

    Assign(Binding, P<ExprNode>),
    Var(Binding),
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

pub struct Parser {
    pub source: Vec<u8>,
    pub tokens: Vec<Token>,
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
            index: 0,
            stack_offset: 0,
            stack_size: 0,
        }
    }

    // parse = stmt*
    pub fn parse(&mut self) -> (Vec<StmtNode>, usize) {
        let mut res = vec![];
        while !self.is_done() {
            res.push(self.stmt());
        }
        self.ensure_done();
        (res, self.stack_size)
    }

    // stmt = expr-stmt
    fn stmt(&mut self) -> StmtNode {
        self.expr_stmt()
    }

    // expr-stmt = expr ";"
    fn expr_stmt(&mut self) -> StmtNode {
        let node = self.expr();
        self.skip(";");
        StmtNode {
            kind: StmtKind::Expr(node.clone()),
            loc: node.loc,
            r#type: node.r#type,
        }
    }

    // expr = equality
    fn expr(&mut self) -> ExprNode {
        self.assign()
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> ExprNode {
        let mut node = self.equality();
        if self.peek().kind == TokenKind::Punct(Punct::Eq) {
            let loc = self.loc();

            // left hand side must be a var
            if let ExprKind::Var(name) = node.kind {
                self.advance();
                node = ExprNode {
                    kind: ExprKind::Assign(name, P::new(self.assign())),
                    loc,
                    r#type: Type::Int,
                };
            }
        }
        node
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> ExprNode {
        let mut node = self.relational();

        while let TokenKind::Punct(punct @ (Punct::EqEq | Punct::Ne)) = self.peek().kind {
            let loc = self.loc();
            self.advance();
            node = ExprNode {
                kind: if punct == Punct::EqEq {
                    ExprKind::Eq(P::new(node), P::new(self.relational()))
                } else {
                    ExprKind::Ne(P::new(node), P::new(self.relational()))
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
            let loc = self.loc();
            self.advance();
            node = ExprNode {
                kind: if punct == Punct::Plus {
                    ExprKind::Add(P::new(node), P::new(self.mul()))
                } else {
                    ExprKind::Sub(P::new(node), P::new(self.mul()))
                },
                loc,
                r#type: Type::Int,
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
            node = ExprNode {
                kind: if punct == Punct::Star {
                    ExprKind::Mul(P::new(node), P::new(self.unary()))
                } else {
                    ExprKind::Div(P::new(node), P::new(self.unary()))
                },
                loc,
                r#type: Type::Int,
            }
        }

        node
    }

    // unary = ("+" | "-") unary
    //       | primary
    fn unary(&mut self) -> ExprNode {
        let loc = self.loc();

        if let TokenKind::Punct(punct @ (Punct::Minus | Punct::Plus)) = self.peek().kind {
            self.advance();
            if punct == Punct::Plus {
                return self.unary();
            }

            return ExprNode {
                kind: ExprKind::Neg(P::new(self.unary())),
                loc,
                r#type: Type::Int,
            };
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
                return ExprNode {
                    kind: ExprKind::Var(Binding {
                        kind: BindingKind::LocalVar {
                            stack_offset: self.stack_offset(),
                        },
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
        self.stack_offset += 8;
        self.align_stack(16);
        -self.stack_offset
    }

    fn align_stack(&mut self, align: usize) {
        self.stack_size = (self.stack_size + align - 1) / align * align;
    }

    fn loc(&self) -> SourceLocation {
        self.peek().loc
    }
}
