use crate::{
    lexer::Keywords,
    parser::{types::Identifier, StatementIterator},
};
use std::collections::VecDeque;

use crate::{
    lexer::{Token, TokenType},
    parser::types::{ASTErrorType, VariableAccess},
};

pub fn parse_identifier(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<Identifier, ASTErrorType> {
    match first_token.token_type {
        TokenType::Identifier(_) => Ok((first_token, false).into()),
        TokenType::Keyword(Keywords::SRS) => match tokens.next() {
            None => Err(ASTErrorType::MissingToken(first_token)),
            Some(
                token @ Token {
                    token_type: TokenType::Identifier(_),
                    ..
                },
            ) => Ok((token, true).into()),
            Some(token) => Err(ASTErrorType::UnexpectedToken(token)),
        },
        _ => Err(ASTErrorType::UnexpectedToken(first_token)),
    }
}

pub fn parse_variable_access(
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
        name: first_ident,
        accesses,
    })
}

pub fn is_valid_variable_access(token: &Token) -> bool {
    matches!(
        token.token_type,
        TokenType::Identifier(_) | TokenType::Keyword(Keywords::SRS)
    )
}
