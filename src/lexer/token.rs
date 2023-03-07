use crate::lexer::Keywords;
use std::fmt::Debug;

use super::Range;

/// Either an integer number or a float number.
#[derive(Clone, Debug, PartialEq)]
pub enum NumberToken {
    /// Integer variant
    Int(i32),
    /// Float variant
    Float(f32),
}

/// All possible literal values in LOLCODE
#[derive(Clone, Debug, PartialEq)]
pub enum TokenValue {
    #[allow(missing_docs)]
    Number(NumberToken),
    #[allow(missing_docs)]
    String(String),
    #[allow(missing_docs)]
    Boolean(bool),
    #[allow(missing_docs)]
    NOOB,
}

/// What type of token this is.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    /// Identifiers are names that aren't LOLCODE keywords.
    Identifier(String),
    /// Keywords are special sequence of characters that has some special meaning in LOLCODE
    Keyword(Keywords),
    /// Symbols are characters that aren't valid identifier names, such as "+", ")", "&", etc...
    Symbol(String),
    /// Values are things like numbers and yarns (strings).
    Value(TokenValue),
    /// A comment single line comment, that is started with the "BTW" token
    CommentSingleLine(String),
    /// A multine comment, which starts with the "OBTW" token and ends with the "TLDR" token.
    CommentMultiLine(String),
    /// When a "'Z" is found, which is used to access a slot in a bukkit.
    BukkitSlotAccess,
    /// When a "..." is found, which is used to extend the statement to the next line.
    Ellipsis,
    /// When a comma (",") is found, which is used to have multiple statements in the same line.
    Comma,
    /// Whe an exclamatin mark is found, which is used with the
    /// [Visible](crate::parser::statements::visible::Visible) statement
    ExclamationMark,
    /// When a question mark is found ("?"), which is used with the
    /// [ORly](crate::parser::statements::o_rly::ORly) statement
    QuestionMark,
}

/// Possible errors that can be found when tokenizing the code string.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenError {
    /// A "OBTW" token was found, but no "TLDR".
    UnclosedMultilineComment(Range),
}

/// A token, which represents a unit of LOLCODE grammar.
#[derive(Clone, PartialEq)]
pub struct Token {
    /// What type of token it is
    pub token_type: TokenType,
    /// The range of characters it spans.
    pub range: Range,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}{:?}", self.range, self.token_type)
    }
}
