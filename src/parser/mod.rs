use crate::lexer::Token;
use statements::parse_statement;

use self::{blocks::parse_block_root, statement_iterator::StatementIterator, types::ASTBlock};

pub mod blocks;
pub mod expression;
pub mod statement_iterator;
pub mod statements;
pub mod types;

pub fn parse(tokens: Vec<Token>) -> ASTBlock {
    let mut line_iterator = StatementIterator::new(tokens);
    return parse_block_root(&mut line_iterator);
}
