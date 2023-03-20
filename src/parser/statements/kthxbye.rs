use crate::lexer::Token;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::Node;

pub(crate) fn parse_kthxbye(first_token: Token) -> Result<Node, ASTErrorType> {
    Ok(Node::KTHXBYE(first_token))
}
