use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::expression::ASTExpression;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::VariableAccess;
use crate::parser::StatementIterator;

/// A Variable Assignment statement
#[derive(Debug, Clone, PartialEq)]
pub struct VariableAssignment {
    /// The variable that will receive the value
    pub variable_access: VariableAccess,
    /// The value that will be put in the variable
    pub expression: ASTExpression,

    pub(crate) r_token: Token,
}

// impl ASTStatement for VariableAccess {
//     fn range(&self) -> crate::lexer::Range {}
// }

/// Errors tha can only happen in a variable assignment statement
#[derive(Debug, PartialEq, Clone)]
pub enum VariableAssignmentError {
    /// When a variable statement is missing the "R" token.
    ///
    /// Ex of error: `var 10`
    /// correct usage: `var R 10`
    MissingR(Token),
    /// When a variable statement has no value expression
    ///
    /// Ex of error: `var R`
    /// correct usage: `var R 10`
    ExpectedValue(Token),
}

impl VariableAssignment {
    pub(crate) fn parse(
        identifier: VariableAccess,
        tokens: &mut StatementIterator,
    ) -> Result<VariableAssignment, ASTErrorType> {
        let r_token = match tokens.next() {
            Some(
                token @ Token {
                    token_type: TokenType::Keyword(Keywords::R),
                    ..
                },
            ) => token,
            _ => {
                return Err(VariableAssignmentError::MissingR(identifier.last_token()).into());
            }
        };

        let value = match tokens.next() {
            Some(initial_token) => ASTExpression::parse(initial_token, tokens)?,
            None => return Err(VariableAssignmentError::ExpectedValue(r_token).into()),
        };

        Ok(VariableAssignment {
            variable_access: identifier,
            expression: value,
            r_token,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::{
        lexer::*,
        parser::expression::{ASTExpressionValue, Identifier},
    };
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn normal() {
        let (identifier, operands) = Token::chain_types(vec![
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::R),
            TokenType::Value(TokenValue::NOOB),
        ]);
        assert_eq!(
            VariableAssignment::parse(
                VariableAccess {
                    identifier: Identifier {
                        name: identifier.clone(),
                        srs: None,
                    },
                    accesses: VecDeque::new()
                },
                &mut StatementIterator::new(operands.clone().into())
            ),
            Ok(VariableAssignment {
                variable_access: VariableAccess {
                    identifier: Identifier {
                        name: identifier.clone(),
                        srs: None,
                    },
                    accesses: VecDeque::new()
                },
                r_token: operands[0].clone(),
                expression: ASTExpression::Value(ASTExpressionValue::LiteralValue(
                    operands[1].clone()
                ))
            })
        );
    }

    #[test]
    fn missing_r() {
        let (identifier, operands) = Token::chain_types(vec![
            TokenType::Identifier("Batata".to_string()),
            TokenType::Value(TokenValue::NOOB),
        ]);
        assert_eq!(
            VariableAssignment::parse(
                VariableAccess {
                    identifier: Identifier {
                        name: identifier.clone(),
                        srs: None,
                    },
                    accesses: VecDeque::new()
                },
                &mut StatementIterator::new(operands.clone().into())
            ),
            Err(VariableAssignmentError::MissingR(identifier).into())
        );
    }
}
