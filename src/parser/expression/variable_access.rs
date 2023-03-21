use crate::lexer::Keywords;
use crate::lexer::Position;
use crate::parser::error::ASTErrorType;
use crate::parser::Identifier;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Token, TokenType};

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
    pub identifier: Identifier,
    /// The other identifiers used after the
    /// BukkitSlotAccess
    pub accesses: VecDeque<Identifier>,
}

impl VariableAccess {
    pub(crate) fn last_token(mut self) -> Token {
        self.accesses
            .pop_back()
            .map(|ident| ident.name)
            .unwrap_or_else(|| self.identifier.name)
    }

    pub(crate) fn range(&self) -> (&Position, &Position) {
        todo!()
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
