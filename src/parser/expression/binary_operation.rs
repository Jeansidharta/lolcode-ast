use crate::{
    lexer::{Keywords, TokenType},
    parser::{error::ASTErrorType, statement_iterator::StatementIterator, Token},
};

use super::{ASTExpression, ASTExpressionIterator, ExpressionError};

/// All binary operators. Each variant represents a different operation. The associated token is
/// a keyword token that generated the operator.
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOpt {
    /// Checks if left or right are true (left || right)
    /// ```LOLCode
    /// EITHER OF left AN right
    /// ```
    EitherOf(Token),
    /// A XOR between the left operant and the right operand. Will be true if left is different.
    /// than right.
    /// ```LOLCode
    /// WON OF left AN right
    /// ```
    WonOf(Token),
    /// Adds both operands as a number (left + right).
    /// ```LOLCode
    /// SUM OF left AN right
    /// ```
    SumOf(Token),
    /// Subtract right from left (left - right).
    /// ```LOLCode
    /// DIFF OF left AN right
    /// ```
    DiffOf(Token),
    /// Multiplies left and right (left * right).
    /// ```LOLCode
    /// DIFF OF left AN right
    /// ```
    ProduktOf(Token),
    /// Divides left and right (left / right).
    /// ```LOLCode
    /// QUOSHUNT OF left AN right
    /// ```
    QuoshuntOf(Token),
    /// Remainder of a division between left and right (left % right).
    /// ```LOLCode
    /// MOD OF left AN right
    /// ```
    ModOf(Token),
    /// Checks if left is bigger than right (left > right).
    /// ```LOLCode
    /// BIGGR OF left AN right
    /// ```
    BiggrOf(Token),
    /// Checks of left is smaller than right (left < right).
    /// ```LOLCode
    /// SMALLR OF left AN right
    /// ```
    SmallrOf(Token),
    /// Checks if left is equal to right (left == right)
    /// ```LOLCode
    /// BOTH SAME left AN right
    /// ```
    BothSaem(Token),
    /// Checks if left and right are both true (left && right)
    /// ```LOLCode
    /// BOTH OF left AN right
    /// ```
    BothOf(Token),
    /// Checks if left is different from right (left != right)
    /// ```LOLCode
    /// DIFFRINT left AN right
    /// ```
    Diffrint(Token),
}

impl BinaryOpt {
    /// Gets a reference to the token inside the operator
    pub fn token(&self) -> &Token {
        match self {
            BinaryOpt::EitherOf(token) => token,
            BinaryOpt::WonOf(token) => token,
            BinaryOpt::SumOf(token) => token,
            BinaryOpt::DiffOf(token) => token,
            BinaryOpt::ProduktOf(token) => token,
            BinaryOpt::QuoshuntOf(token) => token,
            BinaryOpt::ModOf(token) => token,
            BinaryOpt::BiggrOf(token) => token,
            BinaryOpt::SmallrOf(token) => token,
            BinaryOpt::BothSaem(token) => token,
            BinaryOpt::BothOf(token) => token,
            BinaryOpt::Diffrint(token) => token,
        }
    }

    /// Extracts the token from inside the operator, consuming the operator
    pub fn into_token(self) -> Token {
        match self {
            BinaryOpt::EitherOf(token) => token,
            BinaryOpt::WonOf(token) => token,
            BinaryOpt::SumOf(token) => token,
            BinaryOpt::DiffOf(token) => token,
            BinaryOpt::ProduktOf(token) => token,
            BinaryOpt::QuoshuntOf(token) => token,
            BinaryOpt::ModOf(token) => token,
            BinaryOpt::BiggrOf(token) => token,
            BinaryOpt::SmallrOf(token) => token,
            BinaryOpt::BothSaem(token) => token,
            BinaryOpt::BothOf(token) => token,
            BinaryOpt::Diffrint(token) => token,
        }
    }
}

enum BinaryOperationIteratorState<'a> {
    Start,
    Left(ASTExpressionIterator<'a>),
    AnToken,
    Right(ASTExpressionIterator<'a>),
    End,
}

/// Iterator over an BinaryOperation's tokens.
///
/// DOC TODO: show how to instantiate this
pub struct BinaryOperationIterator<'a> {
    operation: &'a BinaryOperation,
    state: BinaryOperationIteratorState<'a>,
}

impl<'a> BinaryOperationIterator<'a> {
    pub(crate) fn new(operation: &'a BinaryOperation) -> Self {
        Self {
            operation,
            state: BinaryOperationIteratorState::Start,
        }
    }
}

impl<'a> Iterator for BinaryOperationIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            BinaryOperationIteratorState::Start => {
                self.state = BinaryOperationIteratorState::Left(self.operation.left.tokens());
                Some(self.operation.operator.token())
            }
            BinaryOperationIteratorState::Left(ref mut expr_iter) => {
                expr_iter.next().or_else(|| {
                    self.state = BinaryOperationIteratorState::AnToken;
                    self.next()
                })
            }
            BinaryOperationIteratorState::AnToken => {
                self.state = BinaryOperationIteratorState::Right(self.operation.right.tokens());
                self.operation.an_token.as_ref().or_else(|| self.next())
            }
            BinaryOperationIteratorState::Right(ref mut expr_iter) => {
                expr_iter.next().or_else(|| {
                    self.state = BinaryOperationIteratorState::End;
                    self.next()
                })
            }
            BinaryOperationIteratorState::End => None,
        }
    }
}

/// A operations with exactly two operands
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOperation {
    pub(crate) left: Box<ASTExpression>,
    pub(crate) operator: BinaryOpt,
    pub(crate) right: Box<ASTExpression>,
    pub(crate) an_token: Option<Token>,
}

impl BinaryOperation {
    /// Returns an iterator over the tokens used to generate this
    pub fn tokens<'a>(&'a self) -> BinaryOperationIterator<'a> {
        BinaryOperationIterator::new(self)
    }

    pub(crate) fn parse(
        operator: BinaryOpt,
        tokens: &mut StatementIterator,
    ) -> Result<ASTExpression, ASTErrorType> {
        let left_first_token = match tokens.next() {
            None => {
                return Err(ASTErrorType::from(ExpressionError::MissingOperands(
                    operator.into_token(),
                )))
            }
            Some(token) => token,
        };
        let left = ASTExpression::parse(left_first_token, tokens)?;
        let an_token =
            tokens.next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::AN)));
        let right_first_token = match tokens.next() {
            None => {
                return Err(ASTErrorType::from(ExpressionError::MissingOperands(
                    operator.into_token(),
                )))
            }
            Some(token) => token,
        };
        let right = ASTExpression::parse(right_first_token, tokens)?;
        Ok(ASTExpression::BinaryOperation(BinaryOperation {
            operator,
            left: Box::new(left),
            right: Box::new(right),
            an_token,
        }))
    }
}
