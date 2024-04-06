use crate::{ErrorReporting, Punct, SourceLocation, Token, TokenKind, Type, P};

#[derive(Debug, Clone, PartialEq)]
pub struct Node<Kind> {
    pub kind: Kind,
    pub loc: SourceLocation,
    pub r#type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Number(i64),
    Add(P<ExprNode>, P<ExprNode>),
    Sub(P<ExprNode>, P<ExprNode>),
    Mul(P<ExprNode>, P<ExprNode>),
    Div(P<ExprNode>, P<ExprNode>),
    Neg(P<ExprNode>),
}

#[derive(Debug, Clone, PartialEq)]
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
        }
    }

    pub fn parse(&mut self) -> Vec<ExprNode> {
        let res = vec![self.expr()];
        self.ensure_done();
        res
    }

    fn expr(&mut self) -> ExprNode {
        self.add()
    }

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

    // primary = (number)* | "(" expr ")"
    fn primary(&mut self) -> ExprNode {
        let loc = self.loc();
        match self.peek().kind {
            TokenKind::Number(val) => {
                self.advance();
                return ExprNode {
                    kind: ExprKind::Number(val),
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
        self.index >= self.tokens.len()
    }

    fn ensure_done(&self) {
        match self.peek().kind {
            TokenKind::Eof => {}
            _ => self.error_tok(self.peek(), "extra token"),
        }
    }

    fn loc(&self) -> SourceLocation {
        self.peek().loc
    }
}
