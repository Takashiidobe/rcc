use crate::ErrorReporting;
#[cfg(test)]
use serde::{Deserialize, Serialize};

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Asm,
    Break,
    Case,
    Const,
    Continue,
    Do,
    Else,
    For,
    Goto,
    If,
    Null,
    Restrict,
    Return,
    Sizeof,
    Struct,
    Switch,
    Typedef,
    Union,
    While,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Macro {
    Define,
    Endif,
    Ifdef,
    Ifndef,
    Include,
    Pragma,
    Undef,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Lifetime {
    Auto,
    Extern,
    Register,
    Static,
    Volatile,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Char,
    Double,
    Float,
    Int,
    Long,
    Unsigned,
    Void,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punct {
    Ampersand,
    Backslash,
    Comma,
    Dot,
    Eq,
    EqEq,
    Gt,
    Gte,
    Hashtag,
    LeftBrace,
    LeftBracket,
    LeftParen,
    Lt,
    Lte,
    Modulo,
    Minus,
    Ne,
    Plus,
    PtrAccess,
    RightBrace,
    RightBracket,
    RightParen,
    Semicolon,
    Slash,
    Star,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword(Keyword),
    Type(TokenType),
    Punct(Punct),
    Lifetime(Lifetime),
    Macro(Macro),
    Number(i64),
    Float(f64),
    Str(String),
    Var(String),
    Eof,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceLocation {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
    pub length: usize,
}

impl ErrorReporting for Tokenizer {
    fn src(&self) -> Vec<u8> {
        self.source.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tokenizer {
    pub source: Vec<u8>,
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl Tokenizer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.bytes().collect(),
            index: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while let Some(c) = self.peek() {
            match c {
                b'"' => tokens.push(self.string()),
                b'0'..=b'9' => tokens.push(self.number()),
                b'_' | b'a'..=b'z' | b'A'..=b'Z' => {
                    tokens.push(self.ident());
                }
                b' ' | b'\t' | b'\r' => {
                    self.advance();
                }
                b'\n' => {
                    self.newline();
                }
                c => {
                    if c == b'/' && self.peek_next() == Some(b'*') {
                        self.advance_n(2);
                        self.handle_multiline_comment();
                        continue;
                    }
                    if c == b'/' && self.peek_next() == Some(b'/') {
                        self.advance_n(2);
                        self.handle_comment();
                        continue;
                    }
                    if self.is_punct(c) {
                        tokens.push(self.punct());
                    } else {
                        panic!("Unhandled byte, {}", c);
                    }
                }
            }
        }
        tokens.push(Token {
            kind: TokenKind::Eof,
            loc: self.location(self.index),
            length: 0,
        });

        tokens
    }

    fn handle_multiline_comment(&mut self) {
        while let (Some(c), Some(n)) = (self.peek(), self.peek_next()) {
            if c == b'*' && n == b'/' {
                break;
            }
            if c == b'\n' {
                self.newline();
            }
            self.advance();
        }
    }

    // handle comments
    fn handle_comment(&mut self) {
        while let Some(c) = self.peek() {
            if c == b'\n' {
                self.newline();
                break;
            } else {
                self.advance();
            }
        }
    }

    fn ident(&mut self) -> Token {
        let start = self.index;

        // macro keywords
        if self.match_n("define") {
            return Token {
                kind: TokenKind::Macro(Macro::Define),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("endif") {
            return Token {
                kind: TokenKind::Macro(Macro::Endif),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("ifdef") {
            return Token {
                kind: TokenKind::Macro(Macro::Ifdef),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("ifndef") {
            return Token {
                kind: TokenKind::Macro(Macro::Ifndef),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("include") {
            return Token {
                kind: TokenKind::Macro(Macro::Include),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("pragma") {
            return Token {
                kind: TokenKind::Macro(Macro::Pragma),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("undef") {
            return Token {
                kind: TokenKind::Macro(Macro::Undef),
                loc: self.location(start),
                length: self.index - start,
            };
        }

        // Types
        if self.match_n("void") {
            return Token {
                kind: TokenKind::Type(TokenType::Void),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("int") {
            return Token {
                kind: TokenKind::Type(TokenType::Int),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("char") {
            return Token {
                kind: TokenKind::Type(TokenType::Char),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("long") {
            return Token {
                kind: TokenKind::Type(TokenType::Long),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("float") {
            return Token {
                kind: TokenKind::Type(TokenType::Float),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("double") {
            return Token {
                kind: TokenKind::Type(TokenType::Double),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("unsigned") {
            return Token {
                kind: TokenKind::Type(TokenType::Unsigned),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        // Lifetimes
        if self.match_n("auto") {
            return Token {
                kind: TokenKind::Lifetime(Lifetime::Auto),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("extern") {
            return Token {
                kind: TokenKind::Lifetime(Lifetime::Extern),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("register") {
            return Token {
                kind: TokenKind::Lifetime(Lifetime::Register),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("static") {
            return Token {
                kind: TokenKind::Lifetime(Lifetime::Static),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("volatile") {
            return Token {
                kind: TokenKind::Lifetime(Lifetime::Volatile),
                loc: self.location(start),
                length: self.index - start,
            };
        }

        // Keywords
        if self.match_n("asm") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Asm),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("break") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Break),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("case") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Case),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("const") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Const),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("continue") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Continue),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("do") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Do),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("else") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Else),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("for") {
            return Token {
                kind: TokenKind::Keyword(Keyword::For),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("if") {
            return Token {
                kind: TokenKind::Keyword(Keyword::If),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("goto") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Goto),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("NULL") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Null),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("restrict") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Restrict),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("return") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Return),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("sizeof") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Sizeof),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("struct") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Struct),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("switch") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Switch),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("typedef") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Typedef),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("union") {
            return Token {
                kind: TokenKind::Keyword(Keyword::Union),
                loc: self.location(start),
                length: self.index - start,
            };
        }
        if self.match_n("while") {
            return Token {
                kind: TokenKind::Keyword(Keyword::While),
                loc: self.location(start),
                length: self.index - start,
            };
        }

        // handle identifiers
        let mut ident = vec![];
        if let Some(c) = self.peek() {
            if c.is_ascii_alphabetic() || c == b'_' {
                ident.push(c);
                self.advance();
            }
        }
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }
        Token {
            kind: TokenKind::Var(String::from_utf8_lossy(&ident).to_string()),
            loc: self.location(start),
            length: self.index - start,
        }
    }

    fn match_n(&mut self, s: &str) -> bool {
        for (i, c) in s.bytes().enumerate() {
            if self.index + i >= self.source.len() || self.source[self.index + i] != c {
                return false;
            }
        }
        self.advance_n(s.len());
        true
    }

    fn is_punct(&self, c: u8) -> bool {
        matches!(
            c,
            b'=' | b'!'
                | b'<'
                | b'>'
                | b'-'
                | b';'
                | b'('
                | b')'
                | b'{'
                | b'}'
                | b','
                | b'.'
                | b'['
                | b']'
                | b'+'
                | b'*'
                | b'/'
                | b'&'
                | b'#'
                | b'%'
                | b'\\'
        )
    }

    fn punct(&mut self) -> Token {
        let start = self.index;
        let c = self.peek().unwrap(); // first char, this is safe
        let next_c = self.peek_next();
        self.advance();
        match (c, next_c) {
            (b'=', Some(b'=')) => {
                self.advance();
                Token {
                    kind: TokenKind::Punct(Punct::EqEq),
                    length: 2,
                    loc: self.location(start),
                }
            }
            (b'!', Some(b'=')) => {
                self.advance();
                Token {
                    kind: TokenKind::Punct(Punct::Ne),
                    length: 2,
                    loc: self.location(start),
                }
            }
            (b'<', Some(b'=')) => {
                self.advance();
                Token {
                    kind: TokenKind::Punct(Punct::Lte),
                    length: 2,
                    loc: self.location(start),
                }
            }
            (b'>', Some(b'=')) => {
                self.advance();
                Token {
                    kind: TokenKind::Punct(Punct::Gte),
                    length: 2,
                    loc: self.location(start),
                }
            }
            (b'-', Some(b'>')) => {
                self.advance();
                Token {
                    kind: TokenKind::Punct(Punct::PtrAccess),
                    length: 2,
                    loc: self.location(start),
                }
            }
            (b';', _) => Token {
                kind: TokenKind::Punct(Punct::Semicolon),
                length: 1,
                loc: self.location(start),
            },
            (b'=', _) => Token {
                kind: TokenKind::Punct(Punct::Eq),
                length: 1,
                loc: self.location(start),
            },
            (b'(', _) => Token {
                kind: TokenKind::Punct(Punct::LeftParen),
                length: 1,
                loc: self.location(start),
            },
            (b')', _) => Token {
                kind: TokenKind::Punct(Punct::RightParen),
                length: 1,
                loc: self.location(start),
            },
            (b'{', _) => Token {
                kind: TokenKind::Punct(Punct::LeftBrace),
                length: 1,
                loc: self.location(start),
            },
            (b'}', _) => Token {
                kind: TokenKind::Punct(Punct::RightBrace),
                length: 1,
                loc: self.location(start),
            },
            (b'[', _) => Token {
                kind: TokenKind::Punct(Punct::LeftBracket),
                length: 1,
                loc: self.location(start),
            },
            (b']', _) => Token {
                kind: TokenKind::Punct(Punct::RightBracket),
                length: 1,
                loc: self.location(start),
            },
            (b',', _) => Token {
                kind: TokenKind::Punct(Punct::Comma),
                length: 1,
                loc: self.location(start),
            },
            (b'.', _) => Token {
                kind: TokenKind::Punct(Punct::Dot),
                length: 1,
                loc: self.location(start),
            },
            (b'+', _) => Token {
                kind: TokenKind::Punct(Punct::Plus),
                length: 1,
                loc: self.location(start),
            },
            (b'-', _) => Token {
                kind: TokenKind::Punct(Punct::Minus),
                length: 1,
                loc: self.location(start),
            },
            (b'*', _) => Token {
                kind: TokenKind::Punct(Punct::Star),
                length: 1,
                loc: self.location(start),
            },
            (b'/', _) => Token {
                kind: TokenKind::Punct(Punct::Slash),
                length: 1,
                loc: self.location(start),
            },
            (b'<', _) => Token {
                kind: TokenKind::Punct(Punct::Lt),
                length: 1,
                loc: self.location(start),
            },
            (b'>', _) => Token {
                kind: TokenKind::Punct(Punct::Gt),
                length: 1,
                loc: self.location(start),
            },
            (b'&', _) => Token {
                kind: TokenKind::Punct(Punct::Ampersand),
                length: 1,
                loc: self.location(start),
            },
            (b'#', _) => Token {
                kind: TokenKind::Punct(Punct::Hashtag),
                length: 1,
                loc: self.location(start),
            },
            (b'%', _) => Token {
                kind: TokenKind::Punct(Punct::Modulo),
                length: 1,
                loc: self.location(start),
            },
            (b'\\', _) => Token {
                kind: TokenKind::Punct(Punct::Backslash),
                length: 1,
                loc: self.location(start),
            },
            _ => unreachable!(),
        }
    }

    fn number(&mut self) -> Token {
        let start = self.index;
        let mut num: i64 = 0;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                num = num * 10 + ((c - b'0') as i64);
                self.advance();
            } else {
                break;
            }
        }

        let end = self.index;

        Token {
            kind: TokenKind::Number(num),
            loc: self.location(start),
            length: end - start,
        }
    }

    fn string(&mut self) -> Token {
        let start = self.index;

        self.expect(b'"');
        let mut s = vec![];

        while let Some(c) = self.peek() {
            if c == b'"' {
                self.advance();
                break;
            } else {
                s.push(c);
                self.advance();
            }
        }
        let end = self.index;

        Token {
            kind: TokenKind::Str(String::from_utf8_lossy(&s).to_string()),
            loc: self.location(start),
            length: end - start - 2,
        }
    }

    fn location(&self, offset: usize) -> SourceLocation {
        SourceLocation {
            offset,
            line: self.line,
            column: self.column,
        }
    }

    fn expect(&mut self, c: u8) {
        if self.peek() != Some(c) {
            panic!("Expected {}, got {:?}", c, self.peek());
        }
        self.advance();
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn advance_n(&mut self, steps: usize) {
        for _ in 0..steps {
            self.index += 1;
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.is_at_end() {
            None
        } else {
            Some(self.source[self.index])
        }
    }

    fn peek_next(&self) -> Option<u8> {
        if self.index + 1 < self.source.len() {
            Some(self.source[self.index + 1])
        } else {
            None
        }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }

    fn newline(&mut self) {
        self.advance();
        self.line += 1;
        self.column = 1;
    }
}

#[macro_export]
macro_rules! kw {
    ($keyword:expr, $kind:expr, $loc:expr, $len:expr) => {
        return Token {
            kind: $kind,
            loc: $loc,
            length: $len,
        };
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_yaml_snapshot;

    #[test]
    fn nothing() {
        let mut tokenizer = Tokenizer::new("");
        assert_yaml_snapshot!(tokenizer.tokenize());
    }

    #[test]
    fn ten() {
        let mut tokenizer = Tokenizer::new("10");
        assert_yaml_snapshot!(tokenizer.tokenize());
    }

    #[test]
    fn multiple() {
        let mut tokenizer = Tokenizer::new("10 20 30");
        assert_yaml_snapshot!(tokenizer.tokenize());
    }

    #[test]
    fn string() {
        let mut tokenizer = Tokenizer::new("\"Hello world\"");
        assert_yaml_snapshot!(tokenizer.tokenize());
    }

    #[test]
    fn with_any_char() {
        let mut tokenizer = Tokenizer::new("\"+-xd\"");
        assert_yaml_snapshot!(tokenizer.tokenize());
    }
    #[test]
    fn multiline_comments() {
        let mut tokenizer = Tokenizer::new(
            r#"// "hello"
            int x = 10;
            "#,
        );
        assert_yaml_snapshot!(tokenizer.tokenize());
    }

    #[test]
    fn main_func_looking() {
        let mut tokenizer = Tokenizer::new("int main() { return 1 + 1; }");
        assert_yaml_snapshot!(tokenizer.tokenize());
    }
}
