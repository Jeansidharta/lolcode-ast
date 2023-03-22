use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::parse_block_loop;
use crate::parser::expression::variable_access::parse_variable_access;
use crate::parser::expression::VariableAccess;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTExpression;
use crate::parser::ASTBlock;
use crate::parser::StatementIterator;

/// Errors tha can only happen when parsing "IM IN YR" expressions
#[derive(Debug, PartialEq, Clone)]
pub enum ImInYrError {
    /// If an operation is provided after the label, the next token must be `YR`. If this is not
    /// the case, this error will be thrown
    ///
    /// Ex of error: `IM IN YR loop UPPIN var`
    /// How it should be: `IM IN YR loop UPPIN YR var`
    MissingYr(Token),
    /// No label for the loop was provided.
    ///
    /// Ex of error: `IM IN YR`
    /// How it should be: `IM IN YR loop`
    MissingStartLabel(Token),
    /// The provided label is not a valid identifier or bukkit access. This will happen if you
    /// provide something like a number or a yarn (string) instead of a variable. Ex: `IM IN YR 10`
    ///
    /// Ex of error: `IM IN YR 10`
    /// How it should be: `IM IN var`
    InvalidStartLabel(Token),
    /// No label provided after `IM OUTTA YR`, which closes the loop.
    ///
    /// Ex of error: `IM OUTTA YR`
    /// How it should be: `IM OUTTA YR loop`
    MissingEndLabel(Token),
    /// The provided label afte `IM OUTTA YR` is not a valid identifier or bukkit access. This will happen if you
    /// provide something like a number or a yarn (string) instead of a variable. Ex: `IM OUTTA YR 10`
    ///
    /// Ex of error: `IM OUTTA YR 10`
    /// How it should be: `IM OUTTA YR loop`
    InvalidEndLabel(Token),
    /// No `IM OUTTA YR' was found for closing the loop.
    MissingImOuttaYr(Token),
    /// The provided operation after the label was invalid.
    ///
    /// Ex of error: `IM IN YR loop ADDING YR var`
    /// How it should be: `IM IN YR loop UPPIN YR var`
    InvalidOperation(Token),
    /// No operand was provided after the operation
    ///
    /// Ex of error: `IM IN YR loop UPPIN YR`
    /// How it should be: `IM IN YR loop UPPIN YR var`
    MissingOperand(Token),
    /// An invalid token was provided after the operand.
    ///
    /// Ex of error: `IM IN YR loop UPPIN YR var BEFORE`
    /// How it should be: `IM IN YR loop UPPIN YR var TIL`
    InvalidConditional(Token),
    /// A valid conditional token was provided, but not expression after
    ///
    /// Ex of error: `IM IN YR loop UPPIN YR var TIL`
    /// How it should be: `IM IN YR loop UPPIN YR var TIL BOTH SAEM var AN 10`
    MissingConditionalExpression(Token),
}

#[derive(Debug, PartialEq, Clone)]
/// The operation used in the loop.
pub(crate) enum LoopOperation {
    /// Will add one to the operand on every iteration
    UPPIN(Token),
    /// Will subract one to the operand on every iteration
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

/// The condition used in the loop.
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum LoopCondition {
    /// Will loop until the given expression is WIN (true)
    TIL(ASTExpression),
    /// Will loop while the given expression is WIN (true)
    WILE(ASTExpression),
}

#[derive(Debug, PartialEq, Clone)]
/// The operation provided for the loop after the label.
pub(crate) struct LoopIterationOperation {
    /// The operation token
    pub(crate) operation: LoopOperation,
    /// The operand
    pub(crate) operand: VariableAccess,
}

/// A statement that will start a loop. This can have 4 forms:
///
/// - Without conditional or operation:
/// ```LOLCODE
/// IM IN YR loop
///     BTW some code here
/// IM OUTTA YR loop
/// ```
///
/// - With conditional but no operation:
/// ```LOLCODE
/// IM IN YR loop TIL <conditional>
///     BTW some code here
/// IM OUTTA YR loop
///
/// - Without conditional but with operation:
/// ```LOLCODE
/// IM IN YR loop UPPIN YR var
///     BTW some code here
/// IM OUTTA YR loop
///
/// - With both conditional and operation:
/// ```LOLCODE
/// IM IN YR loop UPPIN YR var TIL <conditional>
///     BTW some code here
/// IM OUTTA YR loop
#[derive(Debug, Clone, PartialEq)]
pub struct ImInYr {
    pub(crate) im_in_yr_token: Token,
    /// The label provided. Can be any identifier token
    pub(crate) label: Token,
    /// The operation that will run on every iteration
    pub(crate) on_iteration: Option<LoopIterationOperation>,
    /// The condition expression that will determine if a there will be a next loop iteration
    pub(crate) condition: Option<LoopCondition>,
    /// The statements inside the loop.
    pub(crate) code_block: ASTBlock,
    /// The label after the loop closing block
    pub(crate) end_label: Token,
}

impl ImInYr {
    pub(crate) fn parse(
        first_token: Token,
        mut tokens: &mut StatementIterator,
    ) -> Result<ImInYr, ASTErrorType> {
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

        let on_iteration = parse_iteration_operation(&mut tokens)?;
        let condition = parse_conditional(&mut tokens)?;

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
            im_in_yr_token: first_token,
            label,
            on_iteration,
            code_block,
            end_label,
            condition,
        })
    }
}

fn parse_conditional(
    tokens: &mut &mut StatementIterator,
) -> Result<Option<LoopCondition>, ASTErrorType> {
    macro_rules! parse_conditional {
        ($first_token: ident) => {{
            let first_conditional_token = match tokens.next() {
                None => return Err(ImInYrError::MissingConditionalExpression($first_token).into()),
                Some(token) => token,
            };

            ASTExpression::parse(first_conditional_token, tokens)
        }};
    }

    let condition = match tokens.next_if(|t| {
        matches!(
            t.token_type,
            TokenType::Keyword(Keywords::TIL | Keywords::WILE)
        )
    }) {
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
        _ => None,
    };

    Ok(condition)
}

fn parse_iteration_operation(
    tokens: &mut &mut StatementIterator,
) -> Result<Option<LoopIterationOperation>, ASTErrorType> {
    let operation = match tokens.next_if(|t| {
        matches!(
            t.token_type,
            TokenType::Keyword(Keywords::UPPIN | Keywords::NERFIN)
        )
    }) {
        Some(
            token @ Token {
                token_type: TokenType::Keyword(Keywords::UPPIN),
                ..
            },
        ) => LoopOperation::UPPIN(token),
        Some(
            token @ Token {
                token_type: TokenType::Keyword(Keywords::NERFIN),
                ..
            },
        ) => LoopOperation::NERFIN(token),
        _ => return Ok(None),
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

    Ok(Some(LoopIterationOperation { operation, operand }))
}

#[cfg(test)]
mod tests {

    use std::collections::VecDeque;

    use super::*;
    use crate::{
        lexer::{Keywords, NumberToken, TokenType, TokenValue},
        parser::{
            expression::{identifier::Identifier, ASTExpressionValue, BinaryOperation, BinaryOpt},
            statements::{assignment::VariableAssignment, visible::Visible, Node},
        },
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
            ImInYr::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Ok(ImInYr {
                im_in_yr_token: first_token.clone(),
                label: block_tokens[0][0].clone(),
                on_iteration: None,
                condition: None,
                code_block: [
                    Node::Visible(Visible {
                        visible_token: block_tokens[1][0].clone(),
                        expressions: VecDeque::from([
                            ASTExpression::Value(ASTExpressionValue::LiteralValue(
                                block_tokens[1][1].clone()
                            )),
                            ASTExpression::Value(ASTExpressionValue::VariableAccess(
                                VariableAccess {
                                    identifier: Identifier {
                                        name: block_tokens[1][2].clone(),
                                        srs: None,
                                    },
                                    accesses: VecDeque::new(),
                                }
                            ))
                        ]),
                        exclamation_mark: None
                    }),
                    Node::VariableAssignment(VariableAssignment {
                        variable_access: VariableAccess {
                            identifier: Identifier {
                                name: block_tokens[2][0].clone(),
                                srs: None
                            },
                            accesses: VecDeque::new(),
                        },
                        r_token: block_tokens[2][1].clone(),
                        expression: ASTExpression::BinaryOperation(BinaryOperation {
                            operator: BinaryOpt::SumOf(block_tokens[2][2].clone()),
                            left: Box::new(ASTExpression::Value(
                                ASTExpressionValue::VariableAccess(VariableAccess {
                                    identifier: Identifier {
                                        name: block_tokens[2][3].clone(),
                                        srs: None
                                    },
                                    accesses: VecDeque::new(),
                                })
                            )),
                            an_token: Some(block_tokens[2][4].clone()),
                            right: Box::new(ASTExpression::Value(
                                ASTExpressionValue::LiteralValue(block_tokens[2][5].clone())
                            ))
                        })
                    })
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
            ImInYr::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Ok(ImInYr {
                im_in_yr_token: first_token.clone(),
                label: block_tokens[0][0].clone(),
                condition: None,
                on_iteration: Some(LoopIterationOperation {
                    operation: LoopOperation::UPPIN(block_tokens[0][1].clone()),
                    operand: VariableAccess {
                        identifier: Identifier {
                            name: block_tokens[0][3].clone(),
                            srs: None
                        },
                        accesses: VecDeque::new()
                    }
                }),
                code_block: [
                    Visible {
                        visible_token: block_tokens[1][0].clone(),
                        expressions: VecDeque::from([
                            ASTExpression::Value(ASTExpressionValue::LiteralValue(
                                block_tokens[1][1].clone()
                            )),
                            ASTExpression::Value(ASTExpressionValue::VariableAccess(
                                VariableAccess {
                                    identifier: Identifier {
                                        name: block_tokens[1][2].clone(),
                                        srs: None
                                    },
                                    accesses: VecDeque::new()
                                }
                            ))
                        ]),
                        exclamation_mark: None
                    }
                    .into(),
                    VariableAssignment {
                        variable_access: VariableAccess {
                            identifier: Identifier {
                                name: block_tokens[2][0].clone(),
                                srs: None
                            },
                            accesses: VecDeque::new(),
                        },
                        r_token: block_tokens[2][1].clone(),
                        expression: ASTExpression::BinaryOperation(BinaryOperation {
                            operator: BinaryOpt::SumOf(block_tokens[2][2].clone()),
                            left: Box::new(ASTExpression::Value(
                                ASTExpressionValue::VariableAccess(VariableAccess {
                                    identifier: Identifier {
                                        name: block_tokens[2][3].clone(),
                                        srs: None
                                    },
                                    accesses: VecDeque::new(),
                                },)
                            )),
                            an_token: Some(block_tokens[2][4].clone()),
                            right: Box::new(ASTExpression::Value(
                                ASTExpressionValue::LiteralValue(block_tokens[2][5].clone())
                            ))
                        })
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
            ImInYr::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Ok(ImInYr {
                im_in_yr_token: first_token.clone(),
                label: block_tokens[0][0].clone(),
                condition: Some(LoopCondition::TIL(ASTExpression::BinaryOperation(
                    BinaryOperation {
                        operator: BinaryOpt::BothSaem(block_tokens[0][5].clone()),
                        left: Box::new(ASTExpression::Value(ASTExpressionValue::VariableAccess(
                            VariableAccess {
                                identifier: Identifier {
                                    name: block_tokens[0][6].clone(),
                                    srs: None,
                                },
                                accesses: VecDeque::new(),
                            }
                        ))),
                        an_token: Some(block_tokens[0][7].clone()),
                        right: Box::new(ASTExpression::Value(ASTExpressionValue::LiteralValue(
                            block_tokens[0][8].clone()
                        ))),
                    }
                ))),
                on_iteration: Some(LoopIterationOperation {
                    operation: LoopOperation::UPPIN(block_tokens[0][1].clone()),
                    operand: VariableAccess {
                        identifier: Identifier {
                            name: block_tokens[0][3].clone(),
                            srs: None
                        },
                        accesses: VecDeque::new()
                    }
                }),
                code_block: [
                    Visible {
                        visible_token: block_tokens[1][0].clone(),
                        expressions: VecDeque::from([
                            ASTExpression::Value(ASTExpressionValue::LiteralValue(
                                block_tokens[1][1].clone()
                            )),
                            ASTExpression::Value(ASTExpressionValue::VariableAccess(
                                VariableAccess {
                                    identifier: Identifier {
                                        name: block_tokens[1][2].clone(),
                                        srs: None
                                    },
                                    accesses: VecDeque::new(),
                                }
                            ))
                        ]),
                        exclamation_mark: None
                    }
                    .into(),
                    VariableAssignment {
                        variable_access: VariableAccess {
                            identifier: Identifier {
                                name: block_tokens[2][0].clone(),
                                srs: None
                            },
                            accesses: VecDeque::new()
                        },
                        r_token: block_tokens[2][1].clone(),
                        expression: ASTExpression::BinaryOperation(BinaryOperation {
                            operator: BinaryOpt::SumOf(block_tokens[2][2].clone()),
                            left: Box::new(ASTExpression::Value(
                                ASTExpressionValue::VariableAccess(VariableAccess {
                                    identifier: Identifier {
                                        name: block_tokens[2][3].clone(),
                                        srs: None
                                    },
                                    accesses: VecDeque::new(),
                                },)
                            )),
                            an_token: Some(block_tokens[2][4].clone()),
                            right: Box::new(ASTExpression::Value(
                                ASTExpressionValue::LiteralValue(block_tokens[2][5].clone())
                            ))
                        })
                    }
                    .into()
                ]
                .into(),
                end_label: block_tokens[3][1].clone()
            })
        )
    }
}
