use crate::lexer::Token;
use crate::parser::types::{ASTErrorType, ASTNode};

pub fn parse_kthxbye(first_token: Token) -> Result<ASTNode, ASTErrorType> {
    Ok(ASTNode::KTHXBYE(first_token))
}
