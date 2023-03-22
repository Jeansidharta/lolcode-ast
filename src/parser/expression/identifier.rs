use crate::lexer::Token;

/// This represents an Identifier token, an whether it has a SRS before it or not.
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Identifier {
    /// The identifier token
    pub(crate) name: Token,
    /// `Some(token)` if there was a SRS token behind the name. None if there wasn't.
    pub(crate) srs: Option<Token>,
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
