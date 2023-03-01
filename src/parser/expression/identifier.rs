use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub name: Token,
    pub is_srs: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTType {
    Bukkit,
    Numbr,
    Numbar,
    Yarn,
    Troof,
    Noob,
}
