use crate::parser::expression::parse_expression;
use crate::parser::expression::ASTExpression;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::Node;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::{parse_block_if, ASTBlock};

/// The `O RLY` statement
#[derive(Clone, PartialEq, Debug)]
pub struct ORly {
    /// The statements that must be executed if the given expression is true.
    pub if_true: Option<ASTBlock>,
    /// The statements that must be executed if the given expression is false, and no mebbes were
    /// truthful
    pub if_false: Option<ASTBlock>,
    /// The statements that must be executed if the given expression false, but the mebbe
    /// expression is true
    pub mebbes: VecDeque<(ASTExpression, ASTBlock)>,
}

/// Errors that can only happen when parsing a `O RLY?` statement
#[derive(Debug, PartialEq, Clone)]
pub enum ORlyError {
    /// A `O RLY` token was found, but no quesetion mark after it.
    MissingQuestionMark(Token),
    /// A `MEBBE` token was found, but no expression after it
    MissingMebbeExpression(Token),
}

impl TryFrom<(Token, &mut StatementIterator)> for ORly {
    type Error = ASTErrorType;

    fn try_from(
        (first_token, tokens): (Token, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
        match tokens.next().map(|t| t.token_type) {
            Some(TokenType::Symbol(symbol)) if symbol == "?" => {}
            _ => return Err(ORlyError::MissingQuestionMark(first_token).into()),
        };
        tokens.next_statement_should_be_empty()?;

        let if_true = if tokens
            .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::YA_RLY)))
            .is_some()
        {
            tokens.next_statement_should_be_empty()?;
            Some(parse_block_if(tokens))
        } else {
            None
        };

        let mut mebbes = VecDeque::new();
        while let Some(mebbe_token) =
            tokens.next_if_token_type_eq(TokenType::Keyword(Keywords::MEBBE))
        {
            let first_expression_token = match tokens.next() {
                None => return Err(ORlyError::MissingMebbeExpression(mebbe_token).into()),
                Some(token) => token,
            };
            let mebbe_expression = parse_expression(first_expression_token, tokens)?;
            tokens.next_statement_should_be_empty()?;
            mebbes.push_back((mebbe_expression, parse_block_if(tokens)));
        }

        let if_false = if tokens
            .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::NO_WAI)))
            .is_some()
        {
            tokens.next_statement_should_be_empty()?;
            Some(parse_block_if(tokens))
        } else {
            None
        };

        match tokens.next() {
            None => return Err(ASTErrorType::MissingBlockClosingToken(first_token)),
            Some(Token {
                token_type: TokenType::Keyword(Keywords::OIC),
                ..
            }) => {}
            Some(token) => return Err(ASTErrorType::UnexpectedToken(token)),
        }

        Ok(ORly {
            if_true,
            if_false,
            mebbes,
        })
    }
}

impl Into<Node> for ORly {
    fn into(self) -> Node {
        Node::ORly(self)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        lexer::{Keywords, NumberToken, TokenType, TokenValue},
        parser::{
            expression::{ASTExpression, ASTExpressionValue, Identifier, VariableAccess},
            statements::visible::Visible,
        },
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn if_mebbe_else() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::O_RLY),
                TokenType::Symbol("?".to_string()),
            ],
            vec![TokenType::Keyword(Keywords::YA_RLY)],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Identifier("Batata".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::MEBBE),
                TokenType::Value(TokenValue::String("SOME VALUE".to_string())),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::NOOB),
            ],
            vec![
                TokenType::Keyword(Keywords::NO_WAI),
                TokenType::Comma,
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::Number(NumberToken::Int(10))),
            ],
            vec![TokenType::Keyword(Keywords::OIC)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ORly::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(ORly {
                if_true: Some(
                    [Visible(
                        [ASTExpression::Value(ASTExpressionValue::VariableAccess(
                            VariableAccess {
                                identifier: Identifier {
                                    name: block_tokens[2][1].clone(),
                                    srs: None,
                                },
                                accesses: VecDeque::new(),
                            }
                        ))]
                        .into(),
                        None
                    )
                    .into()]
                    .into()
                ),
                mebbes: [(
                    ASTExpression::Value(ASTExpressionValue::LiteralValue(
                        block_tokens[3][1].clone()
                    )),
                    [Visible(
                        [ASTExpression::Value(ASTExpressionValue::LiteralValue(
                            block_tokens[4][1].clone()
                        ))]
                        .into(),
                        None
                    )
                    .into()]
                    .into()
                )]
                .into(),
                if_false: Some(
                    [Visible(
                        [ASTExpression::Value(ASTExpressionValue::LiteralValue(
                            block_tokens[5][3].clone()
                        ))]
                        .into(),
                        None
                    )
                    .into()]
                    .into()
                ),
            })
        )
    }

    #[test]
    fn if_else() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::O_RLY),
                TokenType::Symbol("?".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::NO_WAI),
                TokenType::Comma,
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::Number(NumberToken::Int(10))),
            ],
            vec![TokenType::Keyword(Keywords::OIC)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ORly::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(ORly {
                if_true: None,
                mebbes: VecDeque::new(),
                if_false: Some(ASTBlock(VecDeque::from([Node::Visible(Visible(
                    VecDeque::from([ASTExpression::Value(ASTExpressionValue::LiteralValue(
                        block_tokens[1][3].clone()
                    ))]),
                    None
                ))]))),
            })
        )
    }

    #[test]
    fn missing_question_mark() {
        let mut block_tokens = Token::make_block(vec![
            vec![TokenType::Keyword(Keywords::O_RLY)],
            vec![
                TokenType::Keyword(Keywords::NO_WAI),
                TokenType::Comma,
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::Number(NumberToken::Int(10))),
            ],
            vec![TokenType::Keyword(Keywords::OIC)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ORly::try_from((first_token.clone(), &mut block_tokens.clone().into())),
            Err(ORlyError::MissingQuestionMark(first_token.clone()).into())
        )
    }
}
