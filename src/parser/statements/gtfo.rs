use crate::lexer::Token;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;

pub(crate) fn parse_gtfo(token: Token) -> Result<ASTNode, ASTErrorType> {
    Ok(ASTNode::Gtfo(token))
}
