use crate::parser::Token;

use super::{ASTExpression, ASTExpressionIterator};

/// All operators that can have any number of operands. Each variant represents a different operation. The associated token is
/// a keyword token that generated the operator.
#[derive(Debug, PartialEq, Clone)]
pub enum NaryOpt {
    /// Checks if all operands are true. (A && B && C && ...)
    /// ```LOLCode
    /// ALL OF A AN B AN C MKAY
    /// ```
    AllOf(Token),
    /// Concatenates all string operands
    /// ```LOLCode
    /// SMOOSH A AN B AN C MKAY
    /// ```
    Smoosh(Token),
    /// Checks if any operands are true. (A || B || C || ...)
    /// ```LOLCode
    /// ANY OF A AN B AN C MKAY
    /// ```
    AnyOf(Token),
}

impl NaryOpt {
    /// Extracts the token from inside the operator, consuming the operator
    pub fn into_token(self) -> Token {
        match self {
            NaryOpt::AllOf(token) => token,
            NaryOpt::Smoosh(token) => token,
            NaryOpt::AnyOf(token) => token,
        }
    }

    /// Gets a reference to the token inside the operator
    pub fn token(&self) -> &Token {
        match self {
            NaryOpt::AllOf(token) => token,
            NaryOpt::Smoosh(token) => token,
            NaryOpt::AnyOf(token) => token,
        }
    }
}

enum NaryOperationIteratorState<'a> {
    Start,
    Operand((usize, Option<NaryOperationExpressionIterator<'a>>)),
    Mkay,
    End,
}

/// Iterator over an BinaryOperation's tokens.
///
/// DOC TODO: show how to instantiate this
pub struct NaryOperationIterator<'a> {
    operation: &'a NaryOperation,
    state: NaryOperationIteratorState<'a>,
}

impl<'a> NaryOperationIterator<'a> {
    pub(crate) fn new(operation: &'a NaryOperation) -> Self {
        Self {
            operation,
            state: NaryOperationIteratorState::Start,
        }
    }
}

impl<'a> Iterator for NaryOperationIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            NaryOperationIteratorState::Start => {
                self.state = NaryOperationIteratorState::Operand((0, None));
                Some(self.operation.operator.token())
            }
            NaryOperationIteratorState::Operand((next_index, ref mut iter)) => iter
                .as_mut()
                .and_then(|iter| iter.next())
                .or_else(|| match self.operation.expressions.get(next_index) {
                    Some(expr) => {
                        self.state = NaryOperationIteratorState::Operand((
                            next_index + 1,
                            Some(expr.tokens()),
                        ));
                        self.next()
                    }
                    None => {
                        self.state = NaryOperationIteratorState::Mkay;
                        self.next()
                    }
                }),
            NaryOperationIteratorState::Mkay => {
                self.state = NaryOperationIteratorState::End;
                self.operation.mkay_token.as_ref()
            }
            NaryOperationIteratorState::End => None,
        }
    }
}

enum NaryOperationExpressionIteratorState<'a> {
    Expression(ASTExpressionIterator<'a>),
    AnToken,
}

struct NaryOperationExpressionIterator<'a> {
    expression: &'a NaryOperationOperand,
    state: NaryOperationExpressionIteratorState<'a>,
}

impl<'a> NaryOperationExpressionIterator<'a> {
    fn new(expression: &'a NaryOperationOperand) -> Self {
        Self {
            expression,
            state: NaryOperationExpressionIteratorState::Expression(expression.operand.tokens()),
        }
    }
}

impl<'a> Iterator for NaryOperationExpressionIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            NaryOperationExpressionIteratorState::Expression(ref mut iter) => {
                iter.next().or_else(|| {
                    self.state = NaryOperationExpressionIteratorState::AnToken;
                    self.next()
                })
            }
            NaryOperationExpressionIteratorState::AnToken => self.expression.an_token.as_ref(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct NaryOperationOperand {
    pub(crate) operand: ASTExpression,
    pub(crate) an_token: Option<Token>,
}

impl NaryOperationOperand {
    fn tokens<'a>(&'a self) -> NaryOperationExpressionIterator<'a> {
        NaryOperationExpressionIterator::new(self)
    }
}

/// A operation with any number of operands
#[derive(Debug, PartialEq, Clone)]
pub struct NaryOperation {
    pub(crate) operator: NaryOpt,
    pub(crate) expressions: Vec<NaryOperationOperand>,
    pub(crate) mkay_token: Option<Token>,
}

impl NaryOperation {
    /// Returns an iterator over the tokens used to generate this
    pub fn tokens<'a>(&'a self) -> NaryOperationIterator<'a> {
        NaryOperationIterator::new(self)
    }
}
