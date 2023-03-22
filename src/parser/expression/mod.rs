use crate::lexer::{Keywords, Position, Token, TokenType};
use crate::parser::error::ASTErrorType;
use crate::parser::expression::binary_operation::BinaryOperation;
use crate::parser::expression::nary_operations::NaryOperation;
use crate::parser::expression::nary_operations::NaryOpt;
use crate::parser::expression::unary_operation::UnaryOperation;
use crate::parser::expression::unary_operation::UnaryOpt;
use crate::parser::StatementIterator;

/// Things related to any binary operations, such as `SUM OF a AN b`
pub mod binary_operation;
pub(crate) mod identifier;
mod identifier_iterator;
/// Things related to any Nary operations, such as `SMOOSH a AN b AN c MKAY`
pub mod nary_operations;
/// Things related to any unary operations, such as `NOT b`
pub mod unary_operation;
pub(crate) mod variable_access;
pub(crate) mod variable_access_iterator;

pub use identifier::ASTType;
use identifier::Identifier;
pub use variable_access::VariableAccess;

use self::binary_operation::{BinaryOperationIterator, BinaryOpt};
use self::nary_operations::{NaryOperationIterator, NaryOperationOperand};
use self::unary_operation::UnaryOperationIterator;
use self::variable_access_iterator::VariableAccessIterator;

enum ASTExpressionValueState<'a> {
    Start,
    VariableAccess(VariableAccessIterator<'a>),
    End,
}

/// Iterator over an ASTExpressionValue's tokens.
///
/// DOC TODO: show how to instantiate this
pub struct ASTExpressionValueIterator<'a> {
    value: &'a ASTExpressionValue,
    state: ASTExpressionValueState<'a>,
}

impl<'a> ASTExpressionValueIterator<'a> {
    pub(crate) fn new(value: &'a ASTExpressionValue) -> Self {
        Self {
            value,
            state: ASTExpressionValueState::Start,
        }
    }
}

impl<'a> Iterator for ASTExpressionValueIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            ASTExpressionValueState::Start => match self.value {
                ASTExpressionValue::VariableAccess(var_access) => {
                    self.state = ASTExpressionValueState::VariableAccess(var_access.tokens());
                    self.next()
                }
                ASTExpressionValue::LiteralValue(token) => {
                    self.state = ASTExpressionValueState::End;
                    Some(token)
                }
            },
            ASTExpressionValueState::VariableAccess(ref mut var_access_iter) => {
                match var_access_iter.next() {
                    Some(token) => Some(token),
                    None => {
                        self.state = ASTExpressionValueState::End;
                        self.next()
                    }
                }
            }
            ASTExpressionValueState::End => None,
        }
    }
}

/// The possible values when an expression is a value, not an operation.
#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpressionValue {
    /// The expression is a variable access. It can be either just referencing any variable, or
    /// accessing a bukkit's internal values.
    VariableAccess(VariableAccess),
    /// The expression is just one of the literal values, such as numbers, strings or booleans.
    LiteralValue(Token),
}

impl ASTExpressionValue {
    /// Gets the range of the entire expression
    pub fn range(&self) -> (&Position, &Position) {
        match self {
            ASTExpressionValue::VariableAccess(variable_access) => variable_access.range(),
            ASTExpressionValue::LiteralValue(token) => (&token.range.0, &token.range.1),
        }
    }

    /// Returns an iterator over the tokens used to create this
    pub fn tokens<'a>(&'a self) -> ASTExpressionValueIterator<'a> {
        ASTExpressionValueIterator::new(self)
    }
}

impl From<VariableAccess> for ASTExpressionValue {
    fn from(value: VariableAccess) -> Self {
        ASTExpressionValue::VariableAccess(value)
    }
}

impl TryFrom<Token> for ASTExpressionValue {
    type Error = ();
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.token_type {
            TokenType::Value(_) => Ok(ASTExpressionValue::LiteralValue(value)),
            _ => Err(()),
        }
    }
}

enum ASTExpressionIteratorState<'a> {
    Start,
    Binary(Box<BinaryOperationIterator<'a>>),
    Unary(Box<UnaryOperationIterator<'a>>),
    Nary(Box<NaryOperationIterator<'a>>),
    Value(ASTExpressionValueIterator<'a>),
    End,
}

/// Iterator over an ASTExpression's tokens.
///
/// DOC TODO: show how to instantiate this
pub struct ASTExpressionIterator<'a> {
    expression: &'a ASTExpression,
    state: ASTExpressionIteratorState<'a>,
}

impl<'a> ASTExpressionIterator<'a> {
    pub(crate) fn new(expression: &'a ASTExpression) -> Self {
        Self {
            expression,
            state: ASTExpressionIteratorState::Start,
        }
    }
}

impl<'a> Iterator for ASTExpressionIterator<'a> {
    type Item = &'a Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            ASTExpressionIteratorState::Start => match self.expression {
                ASTExpression::BinaryOperation(opt) => {
                    self.state = ASTExpressionIteratorState::Binary(Box::new(opt.tokens()));
                    self.next()
                }
                ASTExpression::UnaryOperation(opt) => {
                    self.state = ASTExpressionIteratorState::Unary(Box::new(opt.tokens()));
                    self.next()
                }
                ASTExpression::NaryOperation(opt) => {
                    self.state = ASTExpressionIteratorState::Nary(Box::new(opt.tokens()));
                    self.next()
                }
                ASTExpression::Value(value) => {
                    self.state = ASTExpressionIteratorState::Value(value.tokens());
                    self.next()
                }
            },
            ASTExpressionIteratorState::Value(ref mut iter) => iter.next().or_else(|| {
                self.state = ASTExpressionIteratorState::End;
                None
            }),
            ASTExpressionIteratorState::Binary(ref mut iter) => iter.next().or_else(|| {
                self.state = ASTExpressionIteratorState::End;
                None
            }),
            ASTExpressionIteratorState::Unary(ref mut iter) => iter.next().or_else(|| {
                self.state = ASTExpressionIteratorState::End;
                None
            }),
            ASTExpressionIteratorState::Nary(ref mut iter) => iter.next().or_else(|| {
                self.state = ASTExpressionIteratorState::End;
                None
            }),
            ASTExpressionIteratorState::End => None,
        }
    }
}

/// A LOLCODE expression.
#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpression {
    /// The expression is an operation with exactly two operands
    BinaryOperation(BinaryOperation),
    /// The expression is an operation with exactly one operands
    UnaryOperation(UnaryOperation),
    /// The expression is an operation with any number of operands
    NaryOperation(NaryOperation),
    /// A operation with exactly any number of operands
    Value(ASTExpressionValue),
}

impl From<BinaryOperation> for ASTExpression {
    fn from(value: BinaryOperation) -> Self {
        ASTExpression::BinaryOperation(value)
    }
}

impl From<UnaryOperation> for ASTExpression {
    fn from(value: UnaryOperation) -> Self {
        ASTExpression::UnaryOperation(value)
    }
}

impl From<NaryOperation> for ASTExpression {
    fn from(value: NaryOperation) -> Self {
        ASTExpression::NaryOperation(value)
    }
}

impl ASTExpression {
    /// Gets the range (start position and end position) of the entire expression
    pub fn range(&self) -> (&Position, &Position) {
        match self {
            ASTExpression::BinaryOperation(BinaryOperation {
                operator, right, ..
            }) => (&operator.token().range.0, &right.range().1),
            ASTExpression::UnaryOperation(UnaryOperation {
                operator,
                expression,
            }) => (&operator.token().range.0, &expression.range().1),
            ASTExpression::NaryOperation(NaryOperation {
                operator,
                expressions,
                mkay_token,
            }) => (
                &operator.token().range.0,
                mkay_token
                    .as_ref()
                    .map(|t| &t.range.1)
                    .or_else(|| {
                        expressions
                            .last()
                            .map(|NaryOperationOperand { an_token, .. }| {
                                an_token.as_ref().map(|t| &t.range.1)
                            })
                            .flatten()
                    })
                    .or_else(|| {
                        expressions.last().map(
                            |NaryOperationOperand {
                                 operand: expression,
                                 ..
                             }| expression.range().1,
                        )
                    })
                    .unwrap_or_else(|| &operator.token().range.1),
            ),
            ASTExpression::Value(value) => value.range(),
        }
    }

    /// Returns an iterator over the tokens used to generate this
    pub fn tokens<'a>(&'a self) -> ASTExpressionIterator<'a> {
        ASTExpressionIterator::new(self)
    }
}

/// Errors that can only occur inside an expression
#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionError {
    /// Not enough operands were given for the operation
    MissingOperands(Token),
}

trait SwappableResult<T, U> {
    fn swap_ok_err(self) -> Result<U, T>;
}

impl<T, U> SwappableResult<T, U> for Result<T, U> {
    fn swap_ok_err(self) -> Result<U, T> {
        match self {
            Ok(ok) => Err(ok),
            Err(err) => Ok(err),
        }
    }
}

impl ASTExpression {
    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<ASTExpression, ASTErrorType> {
        match first_token.token_type {
            TokenType::Identifier(_) | TokenType::Keyword(Keywords::SRS) => {
                variable_access::VariableAccess::parse(first_token, tokens)
                    .map(|v| ASTExpression::Value(v.into()))
            }
            TokenType::Value(_) => Ok(ASTExpression::Value(ASTExpressionValue::LiteralValue(
                first_token,
            ))),
            TokenType::Keyword(Keywords::NOOB) => Ok(ASTExpression::Value(
                ASTExpressionValue::LiteralValue(first_token),
            )),
            TokenType::Keyword(ref keyword) => {
                match keyword {
                    // Boolean operations
                    Keywords::BOTH_OF => {
                        BinaryOperation::parse(BinaryOpt::BothOf(first_token), tokens)
                    }
                    Keywords::EITHER_OF => {
                        BinaryOperation::parse(BinaryOpt::EitherOf(first_token), tokens)
                    }
                    Keywords::WON_OF => {
                        BinaryOperation::parse(BinaryOpt::WonOf(first_token), tokens)
                    }
                    Keywords::NOT => UnaryOperation::parse(UnaryOpt::Not(first_token), tokens),
                    Keywords::ALL_OF => NaryOperation::parse(NaryOpt::AllOf(first_token), tokens),
                    Keywords::ANY_OF => NaryOperation::parse(NaryOpt::AnyOf(first_token), tokens),
                    // Math operations
                    Keywords::SUM_OF => {
                        BinaryOperation::parse(BinaryOpt::SumOf(first_token), tokens)
                    }
                    Keywords::DIFF_OF => {
                        BinaryOperation::parse(BinaryOpt::DiffOf(first_token), tokens)
                    }
                    Keywords::PRODUKT_OF => {
                        BinaryOperation::parse(BinaryOpt::ProduktOf(first_token), tokens)
                    }
                    Keywords::QUOSHUNT_OF => {
                        BinaryOperation::parse(BinaryOpt::QuoshuntOf(first_token), tokens)
                    }
                    Keywords::MOD_OF => {
                        BinaryOperation::parse(BinaryOpt::ModOf(first_token), tokens)
                    }
                    // Comparison operations
                    Keywords::BIGGR_OF => {
                        BinaryOperation::parse(BinaryOpt::BiggrOf(first_token), tokens)
                    }
                    Keywords::SMALLR_OF => {
                        BinaryOperation::parse(BinaryOpt::SmallrOf(first_token), tokens)
                    }
                    Keywords::BOTH_SAEM => {
                        BinaryOperation::parse(BinaryOpt::BothSaem(first_token), tokens)
                    }
                    Keywords::DIFFRINT => {
                        BinaryOperation::parse(BinaryOpt::Diffrint(first_token), tokens)
                    }
                    // String concat operation
                    Keywords::SMOOSH => NaryOperation::parse(NaryOpt::Smoosh(first_token), tokens),
                    // Type Cast operation
                    Keywords::MAEK => todo!(),

                    _ => Err(ASTErrorType::UnexpectedToken(first_token)),
                }
            }
            _ => Err(ASTErrorType::UnexpectedToken(first_token)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::TokenValue;

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn iterator_simple() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Keyword(Keywords::SUM_OF),
                TokenType::Identifier("A".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Identifier("B".to_string()),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();
        let expression =
            ASTExpression::parse(first_token.clone(), &mut tokens.clone().into()).unwrap();
        let mut iter = expression.tokens();

        assert_eq!(iter.next(), Some(&first_token));
        assert_eq!(iter.next(), Some(&tokens[0]));
        assert_eq!(iter.next(), Some(&tokens[1]));
        assert_eq!(iter.next(), Some(&tokens[2]));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn iterator_unary_binary_nary() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Keyword(Keywords::NOT),
                TokenType::Keyword(Keywords::BOTH_OF),
                TokenType::Keyword(Keywords::SMOOSH),
                TokenType::Identifier("A".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Identifier("B".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Identifier("C".to_string()),
                TokenType::Keyword(Keywords::MKAY),
                TokenType::Keyword(Keywords::AN),
                TokenType::Identifier("D".to_string()),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();
        let expression =
            ASTExpression::parse(first_token.clone(), &mut tokens.clone().into()).unwrap();
        let mut iter = expression.tokens();

        assert_eq!(iter.next(), Some(&first_token));
        assert_eq!(iter.next(), Some(&tokens[0]));
        assert_eq!(iter.next(), Some(&tokens[1]));
        assert_eq!(iter.next(), Some(&tokens[2]));
        assert_eq!(iter.next(), Some(&tokens[3]));
        assert_eq!(iter.next(), Some(&tokens[4]));
        assert_eq!(iter.next(), Some(&tokens[5]));
        assert_eq!(iter.next(), Some(&tokens[6]));
        assert_eq!(iter.next(), Some(&tokens[7]));
        assert_eq!(iter.next(), Some(&tokens[8]));
        assert_eq!(iter.next(), Some(&tokens[9]));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn iterator_nary_access_value() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Keyword(Keywords::ALL_OF),
                TokenType::Keyword(Keywords::SRS),
                TokenType::Identifier("A".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Identifier("B".to_string()),
                TokenType::BukkitSlotAccess,
                TokenType::Identifier("C".to_string()),
                TokenType::BukkitSlotAccess,
                TokenType::Keyword(Keywords::SRS),
                TokenType::Identifier("D".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Value(TokenValue::String("VALUE".to_string())),
                TokenType::Keyword(Keywords::MKAY),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();
        let expression =
            ASTExpression::parse(first_token.clone(), &mut tokens.clone().into()).unwrap();
        let mut iter = expression.tokens();

        assert_eq!(iter.next(), Some(&first_token));
        assert_eq!(iter.next(), Some(&tokens[0]));
        assert_eq!(iter.next(), Some(&tokens[1]));
        assert_eq!(iter.next(), Some(&tokens[2]));
        assert_eq!(iter.next(), Some(&tokens[3]));
        assert_eq!(iter.next(), Some(&tokens[4]));
        assert_eq!(iter.next(), Some(&tokens[5]));
        assert_eq!(iter.next(), Some(&tokens[6]));
        assert_eq!(iter.next(), Some(&tokens[7]));
        assert_eq!(iter.next(), Some(&tokens[8]));
        assert_eq!(iter.next(), Some(&tokens[9]));
        assert_eq!(iter.next(), Some(&tokens[10]));
        assert_eq!(iter.next(), Some(&tokens[11]));
        assert_eq!(iter.next(), None);
    }
}
