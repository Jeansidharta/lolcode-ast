use crate::{
    lexer::{Keywords, TokenType},
    parser::{error::ASTErrorType, statement_iterator::StatementIterator, Token},
};

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
    Operand((usize, Option<NaryOperationOperandIterator<'a>>)),
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

enum NaryOperationOperandIteratorState<'a> {
    Expression(ASTExpressionIterator<'a>),
    AnToken,
    End,
}

struct NaryOperationOperandIterator<'a> {
    expression: &'a NaryOperationOperand,
    state: NaryOperationOperandIteratorState<'a>,
}

impl<'a> NaryOperationOperandIterator<'a> {
    fn new(expression: &'a NaryOperationOperand) -> Self {
        Self {
            expression,
            state: NaryOperationOperandIteratorState::Expression(expression.operand.tokens()),
        }
    }
}

impl<'a> Iterator for NaryOperationOperandIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            NaryOperationOperandIteratorState::Expression(ref mut iter) => {
                iter.next().or_else(|| {
                    self.state = NaryOperationOperandIteratorState::AnToken;
                    self.next()
                })
            }
            NaryOperationOperandIteratorState::AnToken => {
                self.state = NaryOperationOperandIteratorState::End;
                self.expression.an_token.as_ref()
            }
            NaryOperationOperandIteratorState::End => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct NaryOperationOperand {
    pub(crate) operand: ASTExpression,
    pub(crate) an_token: Option<Token>,
}

impl NaryOperationOperand {
    fn tokens<'a>(&'a self) -> NaryOperationOperandIterator<'a> {
        NaryOperationOperandIterator::new(self)
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

    pub(crate) fn parse(
        operator: NaryOpt,
        tokens: &mut StatementIterator,
    ) -> Result<ASTExpression, ASTErrorType> {
        let mut expressions = Vec::new();
        while let Some(token) = tokens.next_if_token_type_ne(TokenType::Keyword(Keywords::MKAY)) {
            let operand = ASTExpression::parse(token, tokens)?;
            let an_token = tokens.next_if_token_type_eq(TokenType::Keyword(Keywords::AN));
            expressions.push(NaryOperationOperand { operand, an_token });
        }
        let mkay_token = tokens.next();
        Ok(ASTExpression::NaryOperation(NaryOperation {
            operator,
            expressions,
            mkay_token,
        }))
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::lexer::{Keywords, TokenType};
    use pretty_assertions::assert_eq;

    #[test]
    fn iterator_smoosh_mkay() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Keyword(Keywords::SMOOSH),
                TokenType::Identifier("A".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Identifier("B".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Identifier("C".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Identifier("D".to_string()),
                TokenType::Keyword(Keywords::MKAY),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();
        let variable_access = NaryOperation::parse(
            NaryOpt::Smoosh(first_token.clone()),
            &mut tokens.clone().into(),
        )
        .unwrap();
        let mut iterator = variable_access.tokens();

        assert_eq!(iterator.next(), Some(&first_token));
        assert_eq!(iterator.next(), Some(&tokens[0]));
        assert_eq!(iterator.next(), Some(&tokens[1]));
        assert_eq!(iterator.next(), Some(&tokens[2]));
        assert_eq!(iterator.next(), Some(&tokens[3]));
        assert_eq!(iterator.next(), Some(&tokens[4]));
        assert_eq!(iterator.next(), Some(&tokens[5]));
        assert_eq!(iterator.next(), Some(&tokens[6]));
        assert_eq!(iterator.next(), Some(&tokens[7]));
        assert_eq!(iterator.next(), None);
    }
}
