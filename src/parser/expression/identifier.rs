use crate::lexer::{Keywords, Position, Token, TokenType};
use crate::parser::error::ASTErrorType;
use crate::parser::expression::identifier_iterator::IdentifierTokensIterator;
use crate::parser::statement_iterator::StatementIterator;

/// This represents an Identifier token, an whether it has a SRS before it or not.
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Identifier {
    /// The identifier token
    pub(crate) name: Token,
    /// `Some(token)` if there was a SRS token behind the name. None if there wasn't.
    pub(crate) srs: Option<Token>,
}

impl Identifier {
    pub fn range(&self) -> (&Position, &Position) {
        (
            match self.srs {
                Some(ref srs) => &srs.range.0,
                None => &self.name.range.0,
            },
            &self.name.range.1,
        )
    }

    pub fn tokens(&self) -> IdentifierTokensIterator {
        IdentifierTokensIterator::new(self)
    }

    pub(crate) fn parse(
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
}

/// These are all the possible types in LOLCODE. This is usefult for the MAEK operation, for
/// example.
#[derive(Debug, PartialEq, Clone)]
pub enum ASTType {
    /// An array
    Bukkit,
    /// An integer number
    Numbr,
    /// A floating number
    Numbar,
    /// A string
    Yarn,
    /// A boolean
    Troof,
    /// An null
    Noob,
}
