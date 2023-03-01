use lolcode_ast;

fn main() {
    lolcode_ast::main().unwrap_or_else(|err| println!("{}", err))
}
