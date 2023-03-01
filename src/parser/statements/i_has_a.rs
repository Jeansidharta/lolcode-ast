use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::expression::variable_access::parse_identifier;
use crate::parser::expression::ASTType;
use crate::parser::expression::Identifier;
use crate::parser::expression::{parse_expression, ASTExpression};
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;
use crate::parser::StatementIterator;

#[derive(Debug, PartialEq, Clone)]
pub enum IHasAInitialValue {
    Expression(ASTExpression),
    Type(ASTType),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IHasAError {
    InvalidIdentifier(Token),
    MissingIdentifier(Token),
    ExpectedInitialValue(Token),
}

impl Into<ASTErrorType> for IHasAError {
    fn into(self) -> ASTErrorType {
        ASTErrorType::IHasA(self)
    }
}

impl From<ASTExpression> for IHasAInitialValue {
    fn from(value: ASTExpression) -> Self {
        IHasAInitialValue::Expression(value)
    }
}

impl From<ASTType> for IHasAInitialValue {
    fn from(value: ASTType) -> Self {
        IHasAInitialValue::Type(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IHasA {
    pub identifier: Identifier,
    pub initial_value: Option<IHasAInitialValue>,
}

impl TryFrom<(Token, &mut StatementIterator)> for IHasA {
    type Error = ASTErrorType;

    fn try_from(
        (first_token, tokens): (Token, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
        let identifier = match tokens.next() {
            None => {
                return Err(IHasAError::MissingIdentifier(first_token).into());
            }
            Some(
                token @ Token {
                    token_type: TokenType::Identifier(_) | TokenType::Keyword(Keywords::SRS),
                    ..
                },
            ) => parse_identifier(token, tokens)?,
            Some(token) => return Err(IHasAError::InvalidIdentifier(token).into()),
        };

        let assignment_token = match tokens.next() {
            None => {
                return Ok(IHasA {
                    identifier,
                    initial_value: None,
                });
            }
            Some(
                token @ Token {
                    token_type: TokenType::Keyword(Keywords::ITZ),
                    ..
                },
            ) => token,
            Some(token) => return Err(ASTErrorType::UnexpectedToken(token)),
        };

        let initial_value = match tokens.next() {
            Some(Token {
                token_type:
                    TokenType::Keyword(
                        token_type @ Keywords::NUMBR
                        | token_type @ Keywords::NUMBAR
                        | token_type @ Keywords::TROOF
                        | token_type @ Keywords::NOOB
                        | token_type @ Keywords::YARN
                        | token_type @ Keywords::BUKKIT,
                    ),
                ..
            }) => match token_type {
                Keywords::NUMBR => IHasAInitialValue::Type(ASTType::Numbr),
                Keywords::NUMBAR => IHasAInitialValue::Type(ASTType::Numbar),
                Keywords::YARN => IHasAInitialValue::Type(ASTType::Yarn),
                Keywords::NOOB => IHasAInitialValue::Type(ASTType::Noob),
                Keywords::BUKKIT => IHasAInitialValue::Type(ASTType::Bukkit),
                Keywords::TROOF => IHasAInitialValue::Type(ASTType::Troof),
                _ => unreachable!(),
            },
            Some(token) => IHasAInitialValue::Expression(parse_expression(token, tokens)?),
            None => return Err(IHasAError::ExpectedInitialValue(assignment_token).into()),
        };

        Ok(IHasA {
            identifier,
            initial_value: Some(initial_value),
        })
    }
}

impl Into<ASTNode> for IHasA {
    fn into(self) -> ASTNode {
        ASTNode::IHasA(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use pretty_assertions::assert_eq;

    impl From<(Identifier, ASTExpression)> for IHasA {
        fn from((identifier, expression): (Identifier, ASTExpression)) -> Self {
            IHasA {
                identifier,
                initial_value: Some(expression.into()),
            }
        }
    }

    impl From<(Identifier, ASTType)> for IHasA {
        fn from((identifier, ast_type): (Identifier, ASTType)) -> Self {
            IHasA {
                identifier,
                initial_value: Some(ast_type.into()),
            }
        }
    }

    impl From<Identifier> for IHasA {
        fn from(identifier: Identifier) -> Self {
            IHasA {
                identifier,
                initial_value: None,
            }
        }
    }

    #[test]
    fn missing_identifier() {
        let (keyword, tokens) = Token::chain_types(vec![TokenType::Keyword(Keywords::I_HAS_A)]);
        assert_eq!(
            IHasA::try_from((keyword.clone(), &mut tokens.clone().into())),
            Err(IHasAError::MissingIdentifier(keyword).into())
        );
    }

    #[test]
    fn invalid_identifier_noob() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::I_HAS_A),
            TokenType::Value(TokenValue::NOOB),
        ]);

        assert_eq!(
            IHasA::try_from((keyword.clone(), &mut tokens.clone().into())),
            Err(IHasAError::InvalidIdentifier(tokens[0].clone()).into())
        );
    }

    #[test]
    fn invalid_identifier_string() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::I_HAS_A),
            TokenType::Value(TokenValue::String("Batata".to_string())),
        ]);

        assert_eq!(
            IHasA::try_from((keyword.clone(), &mut tokens.clone().into())),
            Err(IHasAError::InvalidIdentifier(tokens[0].clone()).into())
        );
    }

    #[test]
    fn invalid_identifier_number() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::I_HAS_A),
            TokenType::Value(TokenValue::Number(NumberToken::Int(123))),
        ]);

        assert_eq!(
            IHasA::try_from((keyword, &mut tokens.clone().into())),
            Err(IHasAError::InvalidIdentifier(tokens[0].clone()).into())
        );
    }

    #[test]
    fn missing_itz() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::I_HAS_A),
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::NOOB),
        ]);

        assert_eq!(
            IHasA::try_from((keyword, &mut tokens.clone().into())),
            Err(ASTErrorType::UnexpectedToken(tokens[1].clone()))
        );
    }

    #[test]
    fn missing_value() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::I_HAS_A),
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::ITZ),
        ]);

        assert_eq!(
            IHasA::try_from((keyword, &mut tokens.clone().into())),
            Err(IHasAError::ExpectedInitialValue(tokens[1].clone()).into())
        );
    }

    #[test]
    fn invalid_value() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::I_HAS_A),
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::ITZ),
            TokenType::Symbol("+".to_string()),
        ]);

        assert_eq!(
            IHasA::try_from((keyword, &mut tokens.clone().into())),
            Err(ASTErrorType::UnexpectedToken(tokens[2].clone()))
        );
    }

    #[test]
    fn success_with_intial_value() {
        let (first_token, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::I_HAS_A),
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::ITZ),
            TokenType::Value(TokenValue::String("Tomate".to_string())),
        ]);

        assert_eq!(
            IHasA::try_from((first_token.clone(), &mut tokens.clone().into())),
            Ok(IHasA {
                identifier: (tokens[0].clone(), false).into(),
                initial_value: Some(ASTExpression::LiteralValue(tokens[2].clone()).into())
            })
        );
    }

    #[test]
    fn success_with_intial_type() {
        let (first_token, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::I_HAS_A),
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::ITZ),
            TokenType::Keyword(Keywords::YARN),
        ]);

        assert_eq!(
            IHasA::try_from((first_token.clone(), &mut tokens.clone().into())),
            Ok(IHasA {
                identifier: (tokens[0].clone(), false).into(),
                initial_value: Some(ASTType::Yarn.into())
            })
        );
    }
}
