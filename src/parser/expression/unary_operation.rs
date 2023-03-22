use crate::parser::Token;

use super::{ASTExpression, ASTExpressionIterator};

/// All unary operators. Each variant represents a different operation. The associated token is
/// a keyword token that generated the operator.
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOpt {
    /// Converts the operand to a boolean, and then negates it.
    /// ```LOLCode
    /// NOT operand
    /// ```
    Not(Token),
}

impl UnaryOpt {
    /// Extracts the token from inside the operator, consuming the operator
    pub fn into_token(self) -> Token {
        match self {
            UnaryOpt::Not(token) => token,
        }
    }

    /// Gets a reference to the token inside the operator
    pub fn token(&self) -> &Token {
        match self {
            UnaryOpt::Not(token) => token,
        }
    }
}

enum UnaryOperationIteratorState<'a> {
    Start,
    Operand(ASTExpressionIterator<'a>),
    End,
}

/// Iterator over an UnaryOperation's tokens.
///
/// DOC TODO: show how to instantiate this
pub struct UnaryOperationIterator<'a> {
    operation: &'a UnaryOperation,
    state: UnaryOperationIteratorState<'a>,
}

impl<'a> UnaryOperationIterator<'a> {
    fn new(operation: &'a UnaryOperation) -> Self {
        Self {
            operation,
            state: UnaryOperationIteratorState::Start,
        }
    }
}

impl<'a> Iterator for UnaryOperationIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            UnaryOperationIteratorState::Start => {
                self.state =
                    UnaryOperationIteratorState::Operand(self.operation.expression.tokens());
                Some(self.operation.operator.token())
            }
            UnaryOperationIteratorState::Operand(ref mut operand_iter) => {
                operand_iter.next().or_else(|| {
                    self.state = UnaryOperationIteratorState::End;
                    None
                })
            }
            UnaryOperationIteratorState::End => None,
        }
    }
}

/// A operation with exactly one operand
#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOperation {
    pub(crate) operator: UnaryOpt,
    pub(crate) expression: Box<ASTExpression>,
}

impl UnaryOperation {
    /// Returns an iterator over the tokens used to generate this
    pub fn tokens<'a>(&'a self) -> UnaryOperationIterator<'a> {
        UnaryOperationIterator::new(self)
    }
}
