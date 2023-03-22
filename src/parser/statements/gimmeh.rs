use crate::lexer::Token;
use crate::parser::expression::variable_access::parse_variable_access;
use crate::parser::expression::VariableAccess;
use crate::parser::statements::ASTErrorType;
use crate::parser::StatementIterator;

/// Represents a `GIMMEH` statement. It gets user input from standard input
///
/// ```LOLCode
/// I HAS A var
/// GIMMEH var
/// ```
#[derive(Debug, PartialEq, Clone)]
pub struct Gimmeh {
    gimmeh_token: Token,
    variable_access: VariableAccess,
}

impl Gimmeh {
    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<Gimmeh, ASTErrorType> {
        match tokens.next() {
            Some(token) => Ok(Gimmeh {
                variable_access: parse_variable_access(token, tokens)?,
                gimmeh_token: first_token,
            }),
            None => Err(ASTErrorType::MissingToken(first_token)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::{
        lexer::{Keywords, TokenType, TokenValue::NOOB},
        parser::expression::{Identifier, VariableAccess},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::GIMMEH),
            TokenType::Identifier("Batata".to_string()),
        ]);

        assert_eq!(
            Gimmeh::parse(keyword.clone(), &mut tokens.clone().into()),
            Ok(Gimmeh {
                gimmeh_token: keyword.clone(),
                variable_access: VariableAccess {
                    identifier: Identifier {
                        name: tokens[0].clone(),
                        srs: None
                    },
                    accesses: VecDeque::new(),
                }
            })
        );
    }

    #[test]
    fn invalid_identifier() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::GIMMEH),
            TokenType::Value(NOOB),
        ]);

        assert_eq!(
            Gimmeh::parse(keyword, &mut tokens.clone().into()),
            Err(ASTErrorType::UnexpectedToken(tokens[0].clone()))
        );
    }

    #[test]
    fn missing_identifier() {
        let (keyword, tokens) = Token::chain_types(vec![TokenType::Keyword(Keywords::GIMMEH)]);

        assert_eq!(
            Gimmeh::parse(keyword.clone(), &mut tokens.clone().into()),
            Err(ASTErrorType::MissingToken(keyword.clone()))
        );
    }
}
