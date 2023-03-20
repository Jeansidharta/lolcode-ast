use crate::lexer::Token;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::Node;

pub(crate) fn parse_gtfo(token: Token) -> Result<Node, ASTErrorType> {
    Ok(Node::Gtfo(token))
}
