use crate::{
    lexer::Token,
    parser::types::{ASTErrorType, ASTNode},
};

pub fn parse_gtfo(token: Token) -> Result<ASTNode, ASTErrorType> {
    Ok(ASTNode::Gtfo(token))
}
