use crate::lexer::Token;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;

pub(crate) fn parse_kthxbye(first_token: Token) -> Result<ASTNode, ASTErrorType> {
    Ok(ASTNode::KTHXBYE(first_token))
}
