use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::expression::variable_access::parse_identifier;
use crate::parser::expression::Identifier;
use crate::parser::expression::{parse_expression, ASTExpression};
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;
use crate::parser::statements::VariableAccess;
use crate::parser::StatementIterator;

use super::assignment::VariableAssignmentError;

/// A statement to set a bukkit slot to a value
#[derive(Clone, Debug, PartialEq)]
pub struct BukkitSetSlot {
    /// The bukkit whose slot will be set
    pub bukkit: VariableAccess,
    /// The bukkit's slot
    pub slot_name: Identifier,
    /// The value that'll be put
    pub value: ASTExpression,
}

impl TryFrom<(VariableAccess, &mut StatementIterator)> for BukkitSetSlot {
    type Error = ASTErrorType;

    fn try_from(
        (bukkit, tokens): (VariableAccess, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
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
            Some(initial_token) => parse_identifier(initial_token, tokens)?,
            None => {
                return Err(ASTErrorType::VariableAssignmentError(
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
            Some(token) => parse_expression(token, tokens)?,
        };

        Ok(BukkitSetSlot {
            bukkit,
            slot_name,
            value,
        })
    }
}

impl Into<ASTNode> for BukkitSetSlot {
    fn into(self) -> ASTNode {
        ASTNode::BukkitSetSlot(self)
    }
}

#[cfg(test)]
mod tests {

    use crate::lexer::*;
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
            BukkitSetSlot::try_from((
                ((identifier.clone(), false), []).into(),
                &mut StatementIterator::new(operands.clone().into())
            )),
            Ok(BukkitSetSlot {
                bukkit: ((identifier.clone(), false), []).into(),
                slot_name: (operands[1].clone(), false).into(),
                value: ASTExpression::LiteralValue(operands[3].clone())
            })
        );
    }
}
