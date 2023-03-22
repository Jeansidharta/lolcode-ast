use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::ast_node::ASTStatement;
use crate::parser::expression::variable_access_iterator::VariableAccessIterator;
use crate::parser::expression::{ASTExpression, ASTExpressionIterator};
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::VariableAccess;
use crate::parser::StatementIterator;

enum VariableAssignmentIteratorState<'a> {
    Start,
    VariableAccess(VariableAccessIterator<'a>),
    RToken,
    Expression(ASTExpressionIterator<'a>),
    End,
}

/// Iterator over an VariableAssignment's tokens.
///
/// DOC TODO: show how to instantiate this
pub struct VariableAssignmentIterator<'a> {
    variable_assignment: &'a VariableAssignment,
    state: VariableAssignmentIteratorState<'a>,
}

impl<'a> VariableAssignmentIterator<'a> {
    pub(crate) fn new(variable_assignment: &'a VariableAssignment) -> Self {
        Self {
            variable_assignment,
            state: VariableAssignmentIteratorState::Start,
        }
    }
}

impl<'a> Iterator for VariableAssignmentIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            VariableAssignmentIteratorState::Start => {
                self.state = VariableAssignmentIteratorState::VariableAccess(
                    self.variable_assignment.variable_access.tokens(),
                );
                self.next()
            }
            VariableAssignmentIteratorState::VariableAccess(ref mut iter) => {
                iter.next().or_else(|| {
                    self.state = VariableAssignmentIteratorState::RToken;
                    self.next()
                })
            }
            VariableAssignmentIteratorState::RToken => {
                self.state = VariableAssignmentIteratorState::Expression(
                    self.variable_assignment.expression.tokens(),
                );
                Some(&self.variable_assignment.r_token)
            }
            VariableAssignmentIteratorState::Expression(ref mut iter) => {
                iter.next().or_else(|| {
                    self.state = VariableAssignmentIteratorState::End;
                    None
                })
            }
            VariableAssignmentIteratorState::End => None,
        }
    }
}

/// A Variable Assignment statement
#[derive(Debug, Clone, PartialEq)]
pub struct VariableAssignment {
    /// The variable that will receive the value
    pub(crate) variable_access: VariableAccess,
    /// The value that will be put in the variable
    pub(crate) expression: ASTExpression,

    pub(crate) r_token: Token,
}

impl<'a> ASTStatement<'a, VariableAssignmentIterator<'a>> for VariableAssignment {
    fn range(&self) -> (&crate::lexer::Position, &crate::lexer::Position) {
        (self.variable_access.range().0, self.expression.range().1)
    }
    fn tokens(&'a self) -> VariableAssignmentIterator<'a> {
        VariableAssignmentIterator::new(self)
    }
    fn similar(&self, _other: &Self) -> bool {
        todo!()
    }
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
        parser::expression::{identifier::Identifier, ASTExpressionValue},
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

    #[test]
    fn iterator_simple_assignment() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Identifier("A".to_string()),
                TokenType::Keyword(Keywords::R),
                TokenType::Keyword(Keywords::NOOB),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();
        let mut statement_iterator: StatementIterator = tokens.clone().into();
        let variable_access =
            VariableAccess::parse(first_token.clone(), &mut statement_iterator).unwrap();
        let expression =
            VariableAssignment::parse(variable_access, &mut statement_iterator).unwrap();
        let mut iter = expression.tokens();

        assert_eq!(iter.next(), Some(&first_token));
        assert_eq!(iter.next(), Some(&tokens[0]));
        assert_eq!(iter.next(), Some(&tokens[1]));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn iterator_expression() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Identifier("A".to_string()),
                TokenType::Keyword(Keywords::R),
                TokenType::Keyword(Keywords::EITHER_OF),
                TokenType::Keyword(Keywords::NOOB),
                TokenType::Keyword(Keywords::AN),
                TokenType::Keyword(Keywords::NOOB),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();
        let mut statement_iterator: StatementIterator = tokens.clone().into();
        let variable_access =
            VariableAccess::parse(first_token.clone(), &mut statement_iterator).unwrap();
        let expression =
            VariableAssignment::parse(variable_access, &mut statement_iterator).unwrap();
        let mut iter = expression.tokens();

        assert_eq!(iter.next(), Some(&first_token));
        assert_eq!(iter.next(), Some(&tokens[0]));
        assert_eq!(iter.next(), Some(&tokens[1]));
        assert_eq!(iter.next(), Some(&tokens[2]));
        assert_eq!(iter.next(), Some(&tokens[3]));
        assert_eq!(iter.next(), Some(&tokens[4]));
        assert_eq!(iter.next(), None);
    }
}
