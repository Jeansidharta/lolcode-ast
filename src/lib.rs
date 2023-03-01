use crate::lexer::tokenize;
use crate::parser::parse;
use std::fs::read_to_string;
use std::path::PathBuf;

mod lexer;
mod lua_transpiler;
mod parser;
mod position;
mod range;

pub fn main() -> Result<(), String> {
    let path = PathBuf::from("batata.lol");
    let file_contents =
        read_to_string(path).or_else(|err| Err(format!("Could not read file: {}", err)))?;
    let tokens = match tokenize(file_contents) {
        Ok(tokens) => tokens,
        Err(err) => return Err(format!("Token error {:#?}", err)),
    };

    let ast = parse(tokens);
    println!("AST {:#?}", ast);

    Ok(())
}
