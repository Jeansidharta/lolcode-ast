use crate::lexer::Keywords;
use crate::lexer::Position;
use crate::parser::error::ASTErrorType;
use crate::parser::expression::Identifier;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Token, TokenType};

use super::variable_access_iterator::{VariableAccessIterator, VariableAccessSlotIterator};

#[derive(Debug, PartialEq, Clone)]
pub struct VariableAccessSlot {
    pub(crate) slot_access_token: Token,
    pub(crate) identifier: Identifier,
}

impl VariableAccessSlot {
    /// Returns an iterator over all tokens used to construct the VariableAccess
    pub fn tokens(&self) -> VariableAccessSlotIterator {
        VariableAccessSlotIterator::new(self)
    }
}

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
    pub(crate) accesses: VecDeque<VariableAccessSlot>,
}

impl VariableAccess {
    pub(crate) fn last_token(mut self) -> Token {
        self.accesses
            .pop_back()
            .map(|VariableAccessSlot { identifier, .. }| identifier.name)
            .unwrap_or_else(|| self.identifier.name)
    }

    /// Gets the range (first and last positions) of the entire Variable access
    pub fn range(&self) -> (&Position, &Position) {
        let identifier_range = self.identifier.range();
        let mut last_position = identifier_range.1;
        match self.accesses.back() {
            Some(VariableAccessSlot { identifier, .. }) => last_position = identifier.range().1,
            None => {}
        };
        (identifier_range.0, last_position)
    }

    /// Returns an iterator over all tokens used to construct the VariableAccess
    pub fn tokens(&self) -> VariableAccessIterator {
        VariableAccessIterator::new(self)
    }

    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<VariableAccess, ASTErrorType> {
        let first_ident = Identifier::parse(first_token, tokens)?;

        let mut accesses = VecDeque::new();
        while let Some(slot_access_token) =
            tokens.next_if(|t| matches!(t.token_type, TokenType::BukkitSlotAccess))
        {
            match tokens.next() {
                Some(token) => accesses.push_back(VariableAccessSlot {
                    slot_access_token,
                    identifier: Identifier::parse(token, tokens)?,
                }),
                None => return Err(ASTErrorType::MissingToken(slot_access_token)),
            }
        }

        Ok(VariableAccess {
            identifier: first_ident,
            accesses,
        })
    }

    pub(crate) fn is_valid(token: &Token) -> bool {
        matches!(
            token.token_type,
            TokenType::Identifier(_) | TokenType::Keyword(Keywords::SRS)
        )
    }
}
