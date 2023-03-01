use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::parse_block_loop;
use crate::parser::expression::parse_expression;
use crate::parser::expression::variable_access::parse_variable_access;
use crate::parser::expression::VariableAccess;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTExpression;
use crate::parser::statements::ASTNode;
use crate::parser::ASTBlock;
use crate::parser::StatementIterator;

#[derive(Debug, PartialEq, Clone)]
pub enum ImInYrError {
    MissingYr(Token),
    MissingStartLabel(Token),
    InvalidStartLabel(Token),
    MissingEndLabel(Token),
    InvalidEndLabel(Token),
    MissingImOuttaYr(Token),
    InvalidOperation(Token),
    MissingOperand(Token),
    InvalidConditional(Token),
    MissingConditionalExpression(Token),
}

impl Into<ASTErrorType> for ImInYrError {
    fn into(self) -> ASTErrorType {
        ASTErrorType::ImInYr(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LoopOperation {
    UPPIN(Token),
    NERFIN(Token),
}

impl Into<Token> for LoopOperation {
    fn into(self) -> Token {
        match self {
            LoopOperation::NERFIN(token) => token,
            LoopOperation::UPPIN(token) => token,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LoopCondition {
    TIL(ASTExpression),
    WILE(ASTExpression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoopIterationOperation {
    pub operation: LoopOperation,
    pub operand: VariableAccess,
    pub condition: Option<LoopCondition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImInYr {
    pub label: Token,
    pub on_iteration: Option<LoopIterationOperation>,
    pub code_block: ASTBlock,
    pub end_label: Token,
}

impl TryFrom<(Token, &mut &mut StatementIterator)> for LoopIterationOperation {
    type Error = ASTErrorType;

    fn try_from(
        (first_token, tokens): (Token, &mut &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
        let operation = match first_token.token_type {
            TokenType::Keyword(Keywords::UPPIN) => LoopOperation::UPPIN(first_token),
            TokenType::Keyword(Keywords::NERFIN) => LoopOperation::NERFIN(first_token),
            _ => unreachable!(),
        };

        match tokens.next() {
            None => return Err(ImInYrError::MissingOperand(operation.into()).into()),
            Some(Token {
                token_type: TokenType::Keyword(Keywords::YR),
                ..
            }) => {}
            Some(token) => return Err(ImInYrError::MissingYr(token).into()),
        }

        let operand = match tokens.next() {
            None => return Err(ImInYrError::MissingOperand(operation.into()).into()),
            Some(token) => parse_variable_access(token, tokens)?,
        };

        macro_rules! parse_conditional {
            ($first_token: ident) => {{
                let first_conditional_token = match tokens.next() {
                    None => {
                        return Err(ImInYrError::MissingConditionalExpression($first_token).into())
                    }
                    Some(token) => token,
                };

                parse_expression(first_conditional_token, tokens)
            }};
        }

        let condition = match tokens.next() {
            None => None,
            Some(
                token @ Token {
                    token_type: TokenType::Keyword(Keywords::TIL),
                    ..
                },
            ) => Some(LoopCondition::TIL(parse_conditional!(token)?)),
            Some(
                token @ Token {
                    token_type: TokenType::Keyword(Keywords::WILE),
                    ..
                },
            ) => Some(LoopCondition::WILE(parse_conditional!(token)?)),
            Some(token) => return Err(ImInYrError::InvalidConditional(token).into()),
        };

        Ok(LoopIterationOperation {
            operation,
            operand,
            condition,
        })
    }
}

impl TryFrom<(Token, &mut StatementIterator)> for ImInYr {
    type Error = ASTErrorType;

    fn try_from(
        (first_token, mut tokens): (Token, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
        let label = match tokens.next() {
            Some(
                token @ Token {
                    token_type: TokenType::Identifier(_),
                    ..
                },
            ) => token,
            None => return Err(ImInYrError::MissingStartLabel(first_token).into()),
            Some(token) => return Err(ImInYrError::InvalidStartLabel(token).into()),
        };

        let on_iteration = match tokens.next() {
            None => None,
            Some(
                token @ Token {
                    token_type: TokenType::Keyword(Keywords::UPPIN | Keywords::NERFIN),
                    ..
                },
            ) => Some(LoopIterationOperation::try_from((token, &mut tokens))?),
            Some(token) => return Err(ImInYrError::InvalidOperation(token).into()),
        };

        tokens.next_statement_should_be_empty()?;

        let code_block = parse_block_loop(tokens);

        let im_outta_yr = match tokens.next() {
            Some(token) => token,
            None => return Err(ImInYrError::MissingImOuttaYr(first_token).into()),
        };

        let end_label = match tokens.next() {
            None => return Err(ImInYrError::MissingEndLabel(im_outta_yr).into()),
            Some(
                token @ Token {
                    token_type: TokenType::Identifier(_),
                    ..
                },
            ) => token,
            Some(token) => return Err(ImInYrError::InvalidEndLabel(token).into()),
        };

        Ok(ImInYr {
            label,
            on_iteration,
            code_block,
            end_label,
        })
    }
}

impl Into<ASTNode> for ImInYr {
    fn into(self) -> ASTNode {
        ASTNode::ImInYr(self)
    }
}

#[cfg(test)]
mod tests {

    use std::{collections::VecDeque, rc::Rc};

    use super::*;
    use crate::{
        lexer::{Keywords, NumberToken, TokenType, TokenValue},
        parser::statements::{assignment::VariableAssignment, visible::Visible},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn no_iteration_operation() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::IM_IN_YR),
                TokenType::Identifier("Batata".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::String("Iteration".to_string())),
                TokenType::Identifier("COUNT".to_string()),
            ],
            vec![
                TokenType::Identifier("COUNT".to_string()),
                TokenType::Keyword(Keywords::R),
                TokenType::Keyword(Keywords::SUM_OF),
                TokenType::Identifier("COUNT".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Value(TokenValue::Number(NumberToken::Int(1))),
            ],
            vec![
                TokenType::Keyword(Keywords::IM_OUTTA_YR),
                TokenType::Identifier("Batata".to_string()),
            ],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ImInYr::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(ImInYr {
                label: block_tokens[0][0].clone(),
                on_iteration: None,
                code_block: [
                    Visible(
                        [
                            ASTExpression::LiteralValue(block_tokens[1][1].clone()),
                            ASTExpression::VariableAccess(
                                ((block_tokens[1][2].clone(), false), []).into()
                            )
                        ]
                        .into(),
                        None
                    )
                    .into(),
                    VariableAssignment {
                        identifier: ((block_tokens[2][0].clone(), false), []).into(),
                        value: ASTExpression::SumOf(
                            Rc::new(ASTExpression::VariableAccess(
                                ((block_tokens[2][3].clone(), false), []).into()
                            )),
                            Rc::new(ASTExpression::LiteralValue(block_tokens[2][5].clone()))
                        )
                    }
                    .into()
                ]
                .into(),
                end_label: block_tokens[3][1].clone()
            })
        )
    }

    #[test]
    fn simple_iteration_uppin() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::IM_IN_YR),
                TokenType::Identifier("Batata".to_string()),
                TokenType::Keyword(Keywords::UPPIN),
                TokenType::Keyword(Keywords::YR),
                TokenType::Identifier("COUNT".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::String("Iteration".to_string())),
                TokenType::Identifier("COUNT".to_string()),
            ],
            vec![
                TokenType::Identifier("COUNT".to_string()),
                TokenType::Keyword(Keywords::R),
                TokenType::Keyword(Keywords::SUM_OF),
                TokenType::Identifier("COUNT".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Value(TokenValue::Number(NumberToken::Int(1))),
            ],
            vec![
                TokenType::Keyword(Keywords::IM_OUTTA_YR),
                TokenType::Identifier("Batata".to_string()),
            ],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ImInYr::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(ImInYr {
                label: block_tokens[0][0].clone(),
                on_iteration: Some(LoopIterationOperation {
                    operation: LoopOperation::UPPIN(block_tokens[0][1].clone()),
                    operand: ((block_tokens[0][3].clone(), false), []).into(),
                    condition: None
                }),
                code_block: [
                    Visible(
                        [
                            ASTExpression::LiteralValue(block_tokens[1][1].clone()),
                            ASTExpression::VariableAccess(
                                ((block_tokens[1][2].clone(), false), []).into()
                            )
                        ]
                        .into(),
                        None
                    )
                    .into(),
                    VariableAssignment {
                        identifier: ((block_tokens[2][0].clone(), false), []).into(),
                        value: ASTExpression::SumOf(
                            Rc::new(ASTExpression::VariableAccess(
                                ((block_tokens[2][3].clone(), false), []).into()
                            )),
                            Rc::new(ASTExpression::LiteralValue(block_tokens[2][5].clone()))
                        )
                    }
                    .into()
                ]
                .into(),
                end_label: block_tokens[3][1].clone()
            })
        )
    }

    #[test]
    fn simple_iteration_uppin_with_limit() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::IM_IN_YR),
                TokenType::Identifier("Batata".to_string()),
                TokenType::Keyword(Keywords::UPPIN),
                TokenType::Keyword(Keywords::YR),
                TokenType::Identifier("COUNT".to_string()),
                TokenType::Keyword(Keywords::TIL),
                TokenType::Keyword(Keywords::BOTH_SAEM),
                TokenType::Identifier("COUNT".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Value(TokenValue::Number(NumberToken::Int(10))),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::String("Iteration".to_string())),
                TokenType::Identifier("COUNT".to_string()),
            ],
            vec![
                TokenType::Identifier("COUNT".to_string()),
                TokenType::Keyword(Keywords::R),
                TokenType::Keyword(Keywords::SUM_OF),
                TokenType::Identifier("COUNT".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Value(TokenValue::Number(NumberToken::Int(1))),
            ],
            vec![
                TokenType::Keyword(Keywords::IM_OUTTA_YR),
                TokenType::Identifier("Batata".to_string()),
            ],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ImInYr::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(ImInYr {
                label: block_tokens[0][0].clone(),
                on_iteration: Some(LoopIterationOperation {
                    operation: LoopOperation::UPPIN(block_tokens[0][1].clone()),
                    operand: ((block_tokens[0][3].clone(), false), []).into(),
                    condition: Some(LoopCondition::TIL(ASTExpression::BothSaem(
                        Rc::new(ASTExpression::VariableAccess(
                            ((block_tokens[0][6].clone(), false), []).into()
                        )),
                        Rc::new(ASTExpression::LiteralValue(block_tokens[0][8].clone())),
                    )))
                }),
                code_block: [
                    Visible(
                        VecDeque::from([
                            ASTExpression::LiteralValue(block_tokens[1][1].clone()),
                            ASTExpression::VariableAccess(
                                ((block_tokens[1][2].clone(), false), []).into()
                            )
                        ]),
                        None
                    )
                    .into(),
                    VariableAssignment {
                        identifier: ((block_tokens[2][0].clone(), false), []).into(),
                        value: ASTExpression::SumOf(
                            Rc::new(ASTExpression::VariableAccess(
                                ((block_tokens[2][3].clone(), false), []).into()
                            )),
                            Rc::new(ASTExpression::LiteralValue(block_tokens[2][5].clone()))
                        )
                    }
                    .into()
                ]
                .into(),
                end_label: block_tokens[3][1].clone()
            })
        )
    }
}
