use lolcode_parser;

fn main() {
    lolcode_parser::main().unwrap_or_else(|err| println!("{}", err))
}
