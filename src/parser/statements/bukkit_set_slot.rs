use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::expression::identifier::Identifier;
use crate::parser::expression::ASTExpression;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::VariableAccess;
use crate::parser::StatementIterator;

use super::assignment::VariableAssignmentError;

/// A statement to set a bukkit slot to a value
#[derive(Clone, Debug, PartialEq)]
pub struct BukkitSetSlot {
    /// The bukkit whose slot will be set
    pub(crate) bukkit: VariableAccess,
    /// The bukkit's slot
    pub(crate) slot_name: Identifier,
    /// The value that'll be put
    pub(crate) value: ASTExpression,
}

impl BukkitSetSlot {
    pub(crate) fn parse(
        bukkit: VariableAccess,
        tokens: &mut StatementIterator,
    ) -> Result<BukkitSetSlot, ASTErrorType> {
        let has_a_token = match tokens.next() {
            Some(
                token @ Token {
                    token_type: TokenType::Keyword(Keywords::HAS_A),
                    ..
                },
            ) => token,
            _ => {
                return Err(ASTErrorType::UnexpectedToken(bukkit.last_token()));
            }
        };

        let slot_name = match tokens.next() {
            Some(initial_token) => Identifier::parse(initial_token, tokens)?,
            None => {
                return Err(ASTErrorType::VariableAssignment(
                    VariableAssignmentError::ExpectedValue(has_a_token),
                ))
            }
        };

        let itz_token = match tokens.next() {
            Some(
                token @ Token {
                    token_type: TokenType::Keyword(Keywords::ITZ),
                    ..
                },
            ) => token,
            Some(token) => return Err(ASTErrorType::UnexpectedToken(token)),
            None => return Err(ASTErrorType::MissingToken(slot_name.name)),
        };

        let value = match tokens.next() {
            None => return Err(ASTErrorType::MissingToken(itz_token)),
            Some(token) => ASTExpression::parse(token, tokens)?,
        };

        Ok(BukkitSetSlot {
            bukkit,
            slot_name,
            value,
        })
    }
}

#[cfg(test)]
mod tests {

    use std::collections::VecDeque;

    use crate::{lexer::*, parser::expression::ASTExpressionValue};
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn normal() {
        let (identifier, operands) = Token::chain_types(vec![
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::HAS_A),
            TokenType::Identifier("Tomate".to_string()),
            TokenType::Keyword(Keywords::ITZ),
            TokenType::Value(TokenValue::String("Cebola".to_string())),
        ]);
        assert_eq!(
            BukkitSetSlot::parse(
                VariableAccess {
                    identifier: Identifier {
                        name: identifier.clone(),
                        srs: None
                    },
                    accesses: VecDeque::new(),
                },
                &mut StatementIterator::new(operands.clone().into())
            ),
            Ok(BukkitSetSlot {
                bukkit: VariableAccess {
                    identifier: Identifier {
                        name: identifier.clone(),
                        srs: None
                    },
                    accesses: VecDeque::new()
                },
                slot_name: Identifier {
                    name: operands[1].clone(),
                    srs: None
                },
                value: ASTExpression::Value(ASTExpressionValue::LiteralValue(operands[3].clone()))
            })
        );
    }
}
