use crate::lexer::{Position, Token};

pub trait ASTStatement<'a, T: Iterator<Item = &'a Token>> {
    fn range(&self) -> (&Position, &Position);

    fn tokens(&'a self) -> T;

    fn similar(&self, other: &Self) -> bool;
}
