use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::expression::{parse_expression, ASTExpression};
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;
use crate::parser::statements::VariableAccess;
use crate::parser::StatementIterator;

/// A Variable Assignment statement
#[derive(Debug, Clone, PartialEq)]
pub struct VariableAssignment {
    /// The variable that will receive the value
    pub identifier: VariableAccess,
    /// The value that will be put in the variable
    pub value: ASTExpression,
}

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

impl TryFrom<(VariableAccess, &mut StatementIterator)> for VariableAssignment {
    type Error = ASTErrorType;

    fn try_from(
        (identifier, tokens): (VariableAccess, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
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
            Some(initial_token) => parse_expression(initial_token, tokens)?,
            None => return Err(VariableAssignmentError::ExpectedValue(r_token).into()),
        };

        Ok(VariableAssignment { identifier, value })
    }
}

impl Into<ASTNode> for VariableAssignment {
    fn into(self) -> ASTNode {
        ASTNode::VariableAssignment(self)
    }
}

impl Into<ASTErrorType> for VariableAssignmentError {
    fn into(self) -> ASTErrorType {
        ASTErrorType::VariableAssignmentError(self)
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
            TokenType::Keyword(Keywords::R),
            TokenType::Value(TokenValue::NOOB),
        ]);
        assert_eq!(
            VariableAssignment::try_from((
                ((identifier.clone(), false), []).into(),
                &mut StatementIterator::new(operands.clone().into())
            )),
            Ok(VariableAssignment {
                identifier: ((identifier.clone(), false), []).into(),
                value: ASTExpression::LiteralValue(operands.get(1).unwrap().clone())
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
            VariableAssignment::try_from((
                ((identifier.clone(), false), []).into(),
                &mut StatementIterator::new(operands.clone().into())
            )),
            Err(VariableAssignmentError::MissingR(identifier).into())
        );
    }
}
