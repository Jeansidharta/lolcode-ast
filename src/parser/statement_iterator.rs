use crate::parser::error::ASTErrorType;
use crate::{lexer::TokenType, parser::Token};
use std::collections::VecDeque;
use std::iter::Peekable;

pub struct StatementIterator {
    tokens_iter: Peekable<std::vec::IntoIter<Token>>,
    current_line: i32,
    peeked: Option<Option<Token>>,
}

impl StatementIterator {
    pub fn new(tokens: Vec<Token>) -> StatementIterator {
        StatementIterator {
            current_line: tokens.first().map(|t| t.range.start_line()).unwrap_or(0),
            tokens_iter: tokens.into_iter().peekable(),
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        match self.peeked {
            Some(ref val) => val.as_ref(),
            None => {
                self.peeked = Some(self.next());
                self.peeked.as_ref().unwrap().as_ref()
            }
        }
    }

    pub fn next_if(&mut self, func: impl FnOnce(&Token) -> bool) -> Option<Token> {
        match self.peek() {
            Some(token) if func(token) => self.next(),
            _ => None,
        }
    }

    pub fn next_if_token_type_eq(&mut self, expected: TokenType) -> Option<Token> {
        match self.peek() {
            Some(token) if token.token_type == expected => self.next(),
            _ => None,
        }
    }

    pub fn next_if_token_type_ne(&mut self, expected: TokenType) -> Option<Token> {
        match self.peek() {
            Some(token) if token.token_type != expected => self.next(),
            _ => None,
        }
    }

    pub fn next_statement_should_be_empty(&mut self) -> Result<(), ASTErrorType> {
        let remaining_tokens = self.next_statement();
        if !remaining_tokens.is_empty() {
            Err(ASTErrorType::TooManyTokens(remaining_tokens))
        } else {
            Ok(())
        }
    }

    pub fn next_statement(&mut self) -> VecDeque<Token> {
        let remaining_tokens = self.collect();
        self.peeked = None;

        while self
            .tokens_iter
            .next_if(|t| t.token_type == TokenType::Comma)
            .is_some()
        {}

        self.tokens_iter
            .peek()
            .map(|t| self.current_line = t.range.start_line());

        remaining_tokens
    }
}

impl From<VecDeque<VecDeque<Token>>> for StatementIterator {
    fn from(value: VecDeque<VecDeque<Token>>) -> Self {
        StatementIterator::new(value.into_iter().flatten().collect())
    }
}

impl From<VecDeque<Token>> for StatementIterator {
    fn from(value: VecDeque<Token>) -> Self {
        StatementIterator::new(value.into())
    }
}

impl From<Vec<Token>> for StatementIterator {
    fn from(value: Vec<Token>) -> Self {
        StatementIterator::new(value)
    }
}

impl<const T: usize> From<[Token; T]> for StatementIterator {
    fn from(value: [Token; T]) -> Self {
        value.to_vec().into()
    }
}

impl Iterator for StatementIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(token) => return token,
            None => {}
        };

        match self.tokens_iter.peek().map(|t| (&t.token_type, &t.range)) {
            None | Some((TokenType::Comma, _)) => None,
            Some((TokenType::Ellipsis, range)) if range.start_line() == self.current_line => {
                self.tokens_iter.next();
                self.tokens_iter
                    .peek()
                    .map(|t| self.current_line = t.range.start_line());
                self.next()
            }
            Some((_, range)) => {
                if range.start_line() == self.current_line {
                    self.tokens_iter.next()
                } else {
                    None
                }
            }
        }
    }
}
