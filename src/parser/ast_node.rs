use crate::lexer::{Position, Token};

pub trait ASTStatement {
    fn range(&self) -> (&Position, &Position);

    fn tokens(&self) -> Vec<&Token>;

    fn similar(&self, other: &Self) -> bool;
}
