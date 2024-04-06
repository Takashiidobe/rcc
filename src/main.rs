use rcc::Tokenizer;

fn main() {
    let mut tokenizer = Tokenizer::new(include_str!("../tests/ex.c"));
    dbg!(tokenizer.tokenize());
}
