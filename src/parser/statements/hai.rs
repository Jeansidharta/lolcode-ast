use crate::parser::statements::ASTErrorType;
use std::collections::VecDeque;

use crate::lexer::Token;
use crate::parser::StatementIterator;

/// A HAI "statement"
#[derive(Debug, Clone, PartialEq)]
pub struct Hai {
    pub(crate) hai_token: Token,
    pub(crate) version: VecDeque<Token>,
}

impl Hai {
    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<Hai, ASTErrorType> {
        Ok(Hai {
            hai_token: first_token,
            version: tokens.next_statement(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Keywords, NumberToken, TokenType, TokenValue};
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let (first_token, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::HAI),
            TokenType::Value(TokenValue::Number(NumberToken::Float(1.4))),
        ]);

        assert_eq!(
            Hai::parse(first_token.clone(), &mut tokens.clone().into()),
            Ok(Hai {
                version: tokens,
                hai_token: first_token.clone()
            })
        );
    }
}
