use rcc::{Codegen, Parser, Tokenizer};
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments", args[0]);
    }

    let src = &args[1];

    let mut tokenizer = Tokenizer::new(src);

    let tokens = tokenizer.tokenize();

    let mut parser = Parser::new(src.as_bytes().to_vec(), tokens);

    let (nodes, stack_size) = parser.parse();

    let mut codegen = Codegen::new(src.as_bytes().to_vec(), nodes, stack_size);
    codegen.program();

    assert!(codegen.depth == 0);
}
