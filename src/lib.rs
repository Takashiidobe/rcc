trait ErrorReporting {
    fn src(&self) -> Vec<u8>;

    fn error_at(&self, offset: usize, msg: &str) -> ! {
        eprintln!("{}", String::from_utf8_lossy(&self.src()));
        eprint!("{: <1$}", "", offset);
        eprintln!("^ {}", msg);
        panic!();
    }

    fn error_tok(&self, tok: &Token, msg: &str) -> ! {
        dbg!(tok);
        self.error_at(tok.loc.offset, msg);
    }
}

type P<T> = Box<T>;

pub mod codegen;
pub mod parser;
pub mod tokenizer;

pub use codegen::*;
pub use parser::*;
pub use tokenizer::*;
