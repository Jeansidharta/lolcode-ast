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
    Bukkit(Token),
    /// An integer number
    Numbr(Token),
    /// A floating number
    Numbar(Token),
    /// A string
    Yarn(Token),
    /// A boolean
    Troof(Token),
    /// An null
    Noob(Token),
}

impl ASTType {
    pub fn token(&self) -> &Token {
        match self {
            ASTType::Bukkit(token) => token,
            ASTType::Numbr(token) => token,
            ASTType::Numbar(token) => token,
            ASTType::Yarn(token) => token,
            ASTType::Troof(token) => token,
            ASTType::Noob(token) => token,
        }
    }

    pub fn into_token(self) -> Token {
        match self {
            ASTType::Bukkit(token) => token,
            ASTType::Numbr(token) => token,
            ASTType::Numbar(token) => token,
            ASTType::Yarn(token) => token,
            ASTType::Troof(token) => token,
            ASTType::Noob(token) => token,
        }
    }
}

impl TryFrom<Token> for ASTType {
    type Error = Token;
    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token.token_type {
            TokenType::Keyword(Keywords::NUMBR) => Ok(ASTType::Numbr(token)),
            TokenType::Keyword(Keywords::NUMBAR) => Ok(ASTType::Numbar(token)),
            TokenType::Keyword(Keywords::YARN) => Ok(ASTType::Yarn(token)),
            TokenType::Keyword(Keywords::NOOB) => Ok(ASTType::Noob(token)),
            TokenType::Keyword(Keywords::BUKKIT) => Ok(ASTType::Bukkit(token)),
            TokenType::Keyword(Keywords::TROOF) => Ok(ASTType::Troof(token)),
            _ => Err(token),
        }
    }
}
