use crate::parser::statements::ASTErrorType;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Token, TokenType};
use crate::parser::expression::ASTExpression;

/// The `VISIBLE` statement. It can accept multiple statements, and an optional "!" at the end to
/// prevent a newline from being printed
#[derive(Debug, Clone, PartialEq)]
pub struct Visible {
    pub(crate) visible_token: Token,
    pub(crate) expressions: VecDeque<ASTExpression>,
    pub(crate) exclamation_mark: Option<Token>,
}

impl Visible {
    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<Visible, ASTErrorType> {
        let mut expressions: VecDeque<ASTExpression> = VecDeque::new();
        while let Some(token) = tokens.next_if_token_type_ne(TokenType::ExclamationMark) {
            expressions.push_back(ASTExpression::parse(token, tokens)?);
        }

        let exclamation_mark = tokens.next_if_token_type_eq(TokenType::ExclamationMark);

        Ok(Visible {
            visible_token: first_token,
            expressions,
            exclamation_mark,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lexer::{Keywords, NumberToken::Int, Token, TokenValue},
        parser::expression::{
            binary_operation::{BinaryOperation, BinaryOpt},
            identifier::Identifier,
            nary_operations::{NaryOperation, NaryOperationOperand, NaryOpt},
            ASTExpressionValue, VariableAccess,
        },
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn success_hello_world() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::VISIBLE),
            TokenType::Value(TokenValue::String("Hello, World!".to_string())),
        ]);

        assert_eq!(
            Visible::parse(keyword.clone(), &mut tokens.clone().into()),
            Ok(Visible {
                visible_token: keyword.clone(),
                expressions: VecDeque::from([ASTExpression::Value(
                    ASTExpressionValue::LiteralValue(tokens[0].clone())
                )]),
                exclamation_mark: None
            })
        );
    }

    #[test]
    fn success_hello_world_exclamation_mark() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::VISIBLE),
            TokenType::Value(TokenValue::String("Hello, World!".to_string())),
            TokenType::ExclamationMark,
        ]);

        assert_eq!(
            Visible::parse(keyword.clone(), &mut tokens.clone().into()),
            Ok(Visible {
                visible_token: keyword.clone(),
                expressions: VecDeque::from([ASTExpression::Value(
                    ASTExpressionValue::LiteralValue(tokens[0].clone())
                )]),
                exclamation_mark: Some(tokens[1].clone())
            })
        );
    }

    #[test]
    fn success_empty_expression() {
        let (keyword, tokens) = Token::chain_types(vec![TokenType::Keyword(Keywords::VISIBLE)]);

        assert_eq!(
            Visible::parse(keyword.clone(), &mut tokens.clone().into()),
            Ok(Visible {
                visible_token: keyword.clone(),
                expressions: VecDeque::new(),
                exclamation_mark: None
            })
        );
    }

    #[test]
    fn success_empty_expression_with_exclamation_mark() {
        let (keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::VISIBLE),
            TokenType::ExclamationMark,
        ]);

        assert_eq!(
            Visible::parse(keyword.clone(), &mut tokens.clone().into()),
            Ok(Visible {
                visible_token: keyword.clone(),
                expressions: VecDeque::new(),
                exclamation_mark: Some(tokens[0].clone())
            })
        );
    }

    #[test]
    fn complex_expressions() {
        let (first_token, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::VISIBLE),
            TokenType::Keyword(Keywords::SMOOSH),
            TokenType::Value(TokenValue::String("Hello, World!".to_string())),
            TokenType::Keyword(Keywords::AN),
            TokenType::Keyword(Keywords::SUM_OF),
            TokenType::Value(TokenValue::Number(Int(10))),
            TokenType::Value(TokenValue::NOOB),
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::MKAY),
            TokenType::Keyword(Keywords::EITHER_OF),
            TokenType::Identifier("Tomate".to_string()),
            TokenType::Keyword(Keywords::AN),
            TokenType::Value(TokenValue::NOOB),
            TokenType::ExclamationMark,
        ]);

        assert_eq!(
            Visible::parse(
                first_token.clone(),
                &mut StatementIterator::new(tokens.clone().into())
            ),
            Ok(Visible {
                visible_token: first_token.clone(),
                expressions: VecDeque::from([
                    ASTExpression::NaryOperation(NaryOperation {
                        operator: NaryOpt::Smoosh(tokens[0].clone()),
                        expressions: vec![
                            NaryOperationOperand {
                                operand: ASTExpression::Value(ASTExpressionValue::LiteralValue(
                                    tokens[1].clone(),
                                )),
                                an_token: Some(tokens[2].clone())
                            },
                            NaryOperationOperand {
                                operand: ASTExpression::BinaryOperation(BinaryOperation {
                                    operator: BinaryOpt::SumOf(tokens[3].clone()),
                                    left: Box::new(ASTExpression::Value(
                                        ASTExpressionValue::LiteralValue(tokens[4].clone())
                                    )),
                                    an_token: None,
                                    right: Box::new(ASTExpression::Value(
                                        ASTExpressionValue::LiteralValue(tokens[5].clone())
                                    ))
                                }),
                                an_token: None
                            },
                            NaryOperationOperand {
                                operand: ASTExpression::Value(ASTExpressionValue::VariableAccess(
                                    VariableAccess {
                                        identifier: Identifier {
                                            name: tokens[6].clone(),
                                            srs: None
                                        },
                                        accesses: VecDeque::new()
                                    }
                                )),
                                an_token: None,
                            }
                        ],
                        mkay_token: Some(tokens[7].clone()),
                    }),
                    ASTExpression::BinaryOperation(BinaryOperation {
                        operator: BinaryOpt::EitherOf(tokens[8].clone()),
                        left: Box::new(ASTExpression::Value(ASTExpressionValue::VariableAccess(
                            VariableAccess {
                                identifier: Identifier {
                                    name: tokens[9].clone(),
                                    srs: None,
                                },
                                accesses: VecDeque::new(),
                            }
                        ))),
                        an_token: Some(tokens[10].clone()),
                        right: Box::new(ASTExpression::Value(ASTExpressionValue::LiteralValue(
                            tokens[11].clone()
                        ))),
                    })
                ]),
                exclamation_mark: Some(tokens[12].clone())
            })
        );
    }
}
