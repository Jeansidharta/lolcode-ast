use crate::lexer::Keywords;
use crate::lexer::Position;
use crate::parser::error::ASTErrorType;
use crate::parser::expression::Identifier;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Token, TokenType};

use super::variable_access_iterator::VariableAccessIterator;

/// A "VariableAccess" represents both a simple variable name, and a bukkit access.
///
/// A simple variable name would be just an [Identifier][1] token in the code.
/// A bukkit access would be an [Identifier][1] token followed by a BukkitSlotAccess and another
/// [Identifier][1].
///
/// [1]: crate::parser::expression::Identifier
#[derive(Debug, PartialEq, Clone)]
pub struct VariableAccess {
    /// The first identifier used.
    pub(crate) identifier: Identifier,
    /// The other identifiers used after the
    /// BukkitSlotAccess
    pub(crate) accesses: VecDeque<Identifier>,
}

impl VariableAccess {
    pub(crate) fn last_token(mut self) -> Token {
        self.accesses
            .pop_back()
            .map(|ident| ident.name)
            .unwrap_or_else(|| self.identifier.name)
    }

    /// Gets the range (first and last positions) of the entire Variable access
    pub fn range(&self) -> (&Position, &Position) {
        let identifier_range = self.identifier.range();
        let mut last_position = identifier_range.1;
        match self.accesses.back() {
            Some(access) => last_position = access.range().1,
            None => {}
        };
        (identifier_range.0, last_position)
    }

    /// Returns an iterator over all tokens used to construct the VariableAccess
    pub fn tokens(&self) -> VariableAccessIterator {
        VariableAccessIterator::new(self)
    }
}

pub(crate) fn parse_identifier(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<Identifier, ASTErrorType> {
    match first_token.token_type {
        TokenType::Identifier(_) => Ok(Identifier {
            name: first_token,
            srs: None,
        }),
        TokenType::Keyword(Keywords::SRS) => match tokens.next() {
            None => Err(ASTErrorType::MissingToken(first_token)),
            Some(
                name_token @ Token {
                    token_type: TokenType::Identifier(_),
                    ..
                },
            ) => Ok(Identifier {
                name: name_token,
                srs: Some(first_token),
            }),
            Some(token) => Err(ASTErrorType::UnexpectedToken(token)),
        },
        _ => Err(ASTErrorType::UnexpectedToken(first_token)),
    }
}

pub(crate) fn parse_variable_access(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<VariableAccess, ASTErrorType> {
    let first_ident = parse_identifier(first_token, tokens)?;

    let mut accesses = VecDeque::new();
    while let Some(slot_acceess_token) =
        tokens.next_if(|t| matches!(t.token_type, TokenType::BukkitSlotAccess))
    {
        match tokens.next() {
            Some(token) => accesses.push_back(parse_identifier(token, tokens)?),
            None => return Err(ASTErrorType::MissingToken(slot_acceess_token)),
        }
    }

    Ok(VariableAccess {
        identifier: first_ident,
        accesses,
    })
}

pub(crate) fn is_valid_variable_access(token: &Token) -> bool {
    matches!(
        token.token_type,
        TokenType::Identifier(_) | TokenType::Keyword(Keywords::SRS)
    )
}
