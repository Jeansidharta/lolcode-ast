use crate::lexer::Token;
use crate::parser::expression::variable_access::parse_variable_access;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::Node;
use crate::parser::StatementIterator;

pub(crate) fn parse_gimmeh(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<Node, ASTErrorType> {
    match tokens.next() {
        Some(token) => {
            parse_variable_access(token, tokens).and_then(|ident| Ok(Node::Gimmeh(ident)))
        }
        None => Err(ASTErrorType::MissingToken(first_token)),
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
            parse_gimmeh(keyword, &mut tokens.clone().into()),
            Ok(Node::Gimmeh(VariableAccess {
                identifier: Identifier {
                    name: tokens[0].clone(),
                    srs: None
                },
                accesses: VecDeque::new(),
            }))
        );
    }

    #[test]
    fn invalid_identifier() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::GIMMEH),
            TokenType::Value(NOOB),
        ]);

        assert_eq!(
            parse_gimmeh(keyword, &mut tokens.clone().into()),
            Err(ASTErrorType::UnexpectedToken(tokens[0].clone()))
        );
    }

    #[test]
    fn missing_identifier() {
        let (keyword, tokens) = Token::chain_types(vec![TokenType::Keyword(Keywords::GIMMEH)]);

        assert_eq!(
            parse_gimmeh(keyword.clone(), &mut tokens.clone().into()),
            Err(ASTErrorType::MissingToken(keyword.clone()))
        );
    }
}
