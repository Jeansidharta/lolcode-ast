use crate::parser::statements::ASTErrorType;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::{parse_block_switch, ASTBlock};
use crate::parser::expression::{parse_expression, ASTExpression};

use super::o_rly::ORlyError;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct WtfOmg {
    pub(crate) omg_token: Token,
    pub(crate) expression: ASTExpression,
    pub(crate) block: ASTBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct WtfOmgWtf {
    pub(crate) omg_wtf_token: Token,
    pub(crate) block: ASTBlock,
}

/// A `WTF?` statement, which is equivalent to a `switch` in other languages
#[derive(Debug, Clone, PartialEq)]
pub struct Wtf {
    pub(crate) wtf_token: Token,
    /// The cases to test the given expression againsT
    pub(crate) omg: VecDeque<WtfOmg>,
    /// If no `OMG` matches, use the `OMGWTF`
    pub(crate) omg_wtf: Option<WtfOmgWtf>,
}

impl Wtf {
    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<Wtf, ASTErrorType> {
        match tokens.next().map(|t| t.token_type) {
            Some(TokenType::Symbol(symbol)) if symbol == "?" => {}
            _ => return Err(ORlyError::MissingQuestionMark(first_token).into()),
        };
        tokens.next_statement_should_be_empty()?;

        let mut omg = VecDeque::new();
        while let Some(omg_token) =
            tokens.next_if(|token| matches!(&token.token_type, TokenType::Keyword(Keywords::OMG)))
        {
            let first_token = match tokens.next() {
                None => return Err(ASTErrorType::MissingToken(omg_token)),
                Some(token) => token,
            };
            let expression = parse_expression(first_token, tokens)?;
            tokens.next_statement_should_be_empty()?;
            omg.push_back(WtfOmg {
                omg_token,
                expression,
                block: parse_block_switch(tokens),
            });
        }

        let omg_wtf = if let Some(omg_wtf_token) =
            tokens.next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::OMGWTF)))
        {
            tokens.next_statement_should_be_empty()?;
            Some(WtfOmgWtf {
                block: parse_block_switch(tokens),
                omg_wtf_token,
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

        tokens.next_statement_should_be_empty()?;

        Ok(Wtf {
            wtf_token: first_token,
            omg,
            omg_wtf,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        lexer::{Keywords, TokenType, TokenValue},
        parser::{
            expression::{ASTExpressionValue, Identifier, VariableAccess},
            statements::{visible::Visible, Node},
        },
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_switch() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::WTF),
                TokenType::Symbol("?".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::OMG),
                TokenType::Value(TokenValue::Boolean(true)),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Identifier("WIN!!1!".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::OMG),
                TokenType::Value(TokenValue::Boolean(false)),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Identifier("FAIL :>(".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::OMGWTF),
                TokenType::Comma,
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::NOOB),
            ],
            vec![TokenType::Keyword(Keywords::OIC)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            Wtf::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Ok(Wtf {
                wtf_token: first_token.clone(),
                omg: VecDeque::from([
                    WtfOmg {
                        omg_token: block_tokens[1][0].clone(),
                        expression: ASTExpression::Value(ASTExpressionValue::LiteralValue(
                            block_tokens[1][1].clone(),
                        )),
                        block: ASTBlock(VecDeque::from([Node::Visible(Visible {
                            visible_token: block_tokens[2][0].clone(),
                            expressions: VecDeque::from([ASTExpression::Value(
                                ASTExpressionValue::VariableAccess(VariableAccess {
                                    identifier: Identifier {
                                        name: block_tokens[2][1].clone(),
                                        srs: None,
                                    },
                                    accesses: VecDeque::new()
                                })
                            )]),
                            exclamation_mark: None
                        })])),
                    },
                    WtfOmg {
                        omg_token: block_tokens[3][0].clone(),
                        expression: ASTExpression::Value(ASTExpressionValue::LiteralValue(
                            block_tokens[3][1].clone(),
                        )),
                        block: ASTBlock(VecDeque::from([Node::Visible(Visible {
                            visible_token: block_tokens[4][0].clone(),
                            expressions: VecDeque::from([ASTExpression::Value(
                                ASTExpressionValue::VariableAccess(VariableAccess {
                                    identifier: Identifier {
                                        name: block_tokens[4][1].clone(),
                                        srs: None,
                                    },
                                    accesses: VecDeque::new()
                                })
                            )]),
                            exclamation_mark: None
                        })])),
                    }
                ]),
                omg_wtf: Some(WtfOmgWtf {
                    omg_wtf_token: block_tokens[5][0].clone(),
                    block: ASTBlock(VecDeque::from([Node::Visible(Visible {
                        visible_token: block_tokens[5][2].clone(),
                        expressions: VecDeque::from([ASTExpression::Value(
                            ASTExpressionValue::LiteralValue(block_tokens[5][3].clone())
                        )]),
                        exclamation_mark: None
                    })]))
                })
            })
        )
    }
}
