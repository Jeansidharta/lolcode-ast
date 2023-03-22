use crate::parser::expression::ASTExpression;
use crate::parser::statements::ASTErrorType;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::{parse_block_if, ASTBlock};

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct YaRly {
    pub(crate) ya_rly_token: Token,
    pub(crate) block: ASTBlock,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct NoWai {
    pub(crate) no_wai_token: Token,
    pub(crate) block: ASTBlock,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Mebbe {
    pub(crate) mebbe_token: Token,
    pub(crate) condition: ASTExpression,
    pub(crate) block: ASTBlock,
}

/// The `O RLY` statement
#[derive(Clone, PartialEq, Debug)]
pub struct ORly {
    o_rly_token: Token,
    /// The statements that must be executed if the given expression is true.
    pub(crate) if_true: Option<YaRly>,
    /// The statements that must be executed if the given expression is false, and no mebbes were
    /// truthful
    pub(crate) if_false: Option<NoWai>,
    /// The statements that must be executed if the given expression false, but the mebbe
    /// expression is true
    pub(crate) mebbes: VecDeque<Mebbe>,
}

/// Errors that can only happen when parsing a `O RLY?` statement
#[derive(Debug, PartialEq, Clone)]
pub enum ORlyError {
    /// A `O RLY` token was found, but no quesetion mark after it.
    MissingQuestionMark(Token),
    /// A `MEBBE` token was found, but no expression after it
    MissingMebbeExpression(Token),
}

impl ORly {
    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<ORly, ASTErrorType> {
        match tokens.next().map(|t| t.token_type) {
            Some(TokenType::Symbol(symbol)) if symbol == "?" => {}
            _ => return Err(ORlyError::MissingQuestionMark(first_token).into()),
        };
        tokens.next_statement_should_be_empty()?;

        let if_true = if let Some(ya_rly_token) =
            tokens.next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::YA_RLY)))
        {
            tokens.next_statement_should_be_empty()?;
            Some(YaRly {
                ya_rly_token,
                block: parse_block_if(tokens),
            })
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
            let condition = ASTExpression::parse(first_expression_token, tokens)?;
            tokens.next_statement_should_be_empty()?;
            mebbes.push_back(Mebbe {
                mebbe_token,
                condition,
                block: parse_block_if(tokens),
            });
        }

        let if_false = if let Some(no_wai_token) =
            tokens.next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::NO_WAI)))
        {
            tokens.next_statement_should_be_empty()?;
            Some(NoWai {
                no_wai_token,
                block: parse_block_if(tokens),
            })
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
            o_rly_token: first_token,
            if_true,
            if_false,
            mebbes,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        lexer::{Keywords, NumberToken, TokenType, TokenValue},
        parser::{
            expression::{ASTExpression, ASTExpressionValue, Identifier, VariableAccess},
            statements::{visible::Visible, Node},
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
            ORly::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Ok(ORly {
                o_rly_token: first_token.clone(),
                if_true: Some(YaRly {
                    ya_rly_token: block_tokens[1][0].clone(),
                    block: ASTBlock(VecDeque::from([Node::Visible(Visible {
                        visible_token: block_tokens[2][0].clone(),
                        expressions: VecDeque::from([ASTExpression::Value(
                            ASTExpressionValue::VariableAccess(VariableAccess {
                                identifier: Identifier {
                                    name: block_tokens[2][1].clone(),
                                    srs: None,
                                },
                                accesses: VecDeque::new(),
                            })
                        )]),
                        exclamation_mark: None
                    })]))
                }),
                mebbes: VecDeque::from([Mebbe {
                    mebbe_token: block_tokens[3][0].clone(),
                    condition: ASTExpression::Value(ASTExpressionValue::LiteralValue(
                        block_tokens[3][1].clone()
                    )),
                    block: ASTBlock(VecDeque::from([Node::Visible(Visible {
                        visible_token: block_tokens[4][0].clone(),
                        expressions: VecDeque::from([ASTExpression::Value(
                            ASTExpressionValue::LiteralValue(block_tokens[4][1].clone())
                        )]),
                        exclamation_mark: None
                    })]))
                }]),
                if_false: Some(NoWai {
                    no_wai_token: block_tokens[5][0].clone(),
                    block: ASTBlock(VecDeque::from([Node::Visible(Visible {
                        visible_token: block_tokens[5][2].clone(),
                        expressions: VecDeque::from([ASTExpression::Value(
                            ASTExpressionValue::LiteralValue(block_tokens[5][3].clone())
                        )]),
                        exclamation_mark: None
                    })]))
                }),
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
            ORly::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Ok(ORly {
                o_rly_token: first_token.clone(),
                if_true: None,
                mebbes: VecDeque::new(),
                if_false: Some(NoWai {
                    no_wai_token: block_tokens[1][0].clone(),
                    block: ASTBlock(VecDeque::from([Node::Visible(Visible {
                        visible_token: block_tokens[1][2].clone(),
                        expressions: VecDeque::from([ASTExpression::Value(
                            ASTExpressionValue::LiteralValue(block_tokens[1][3].clone())
                        )]),
                        exclamation_mark: None
                    })]))
                }),
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
            ORly::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Err(ORlyError::MissingQuestionMark(first_token.clone()).into())
        )
    }
}
