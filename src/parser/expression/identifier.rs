use crate::lexer::{Token, TokenType};

/// This represents an Identifier token, an whether it has a SRS before it or not.
#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    /// The identifier token
    pub name: Token,
    /// If there was a SRS token before this identifier.
    pub is_srs: bool,
}

impl Identifier {
    /// Gets a string representation of the identifier
    pub fn to_string_slice(&self) -> &str {
        match self.name.token_type {
            TokenType::Identifier(ref str) => str,
            _ => unreachable!(),
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
