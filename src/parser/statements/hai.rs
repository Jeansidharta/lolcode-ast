use crate::parser::statements::ASTErrorType;
use crate::parser::statements::Node;
use std::collections::VecDeque;

use crate::lexer::Token;
use crate::parser::StatementIterator;

/// A HAI "statement"
#[derive(Debug, Clone, PartialEq)]
pub struct Hai(pub VecDeque<Token>);

impl Into<Node> for Hai {
    fn into(self) -> Node {
        Node::HAI(self)
    }
}

impl TryFrom<&mut StatementIterator> for Hai {
    type Error = ASTErrorType;
    fn try_from(tokens: &mut StatementIterator) -> Result<Self, Self::Error> {
        Ok(Hai(tokens.next_statement()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Keywords, NumberToken, TokenType, TokenValue};
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let (_, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::HAI),
            TokenType::Value(TokenValue::Number(NumberToken::Float(1.4))),
        ]);

        assert_eq!(Hai::try_from(&mut tokens.clone().into()), Ok(Hai(tokens)));
    }
}
