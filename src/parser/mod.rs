use crate::lexer::Token;
use statements::parse_statement;

use self::{blocks::parse_block_root, statement_iterator::StatementIterator};

pub use blocks::ASTBlock;

mod ast_node;
mod blocks;
mod error;
/// Expression related data structures and logic.
pub mod expression;
mod statement_iterator;
/// Statement related data structures and logic
pub mod statements;

/// Transforms an array of tokens into an ASTBlock, which is an array of statements.
pub fn parse(tokens: Vec<Token>) -> ASTBlock {
    let mut line_iterator = StatementIterator::new(tokens);
    return parse_block_root(&mut line_iterator);
}
