use std::{
    fmt::{self, Debug, Display, Formatter},
    ops::{Add, AddAssign},
};

/// The Position struct represents the position of a character in the text.
///
/// Note that the bytes field and chars field may differ if the text has non-ascii characters
#[derive(Clone, Copy)]
pub struct Position {
    /// The line of the character
    pub line: i32,
    /// The column of the character
    pub column: i32,
    /// The initial byte of the character
    pub bytes: i32,
    /// The character index
    pub chars: i32,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(L{}, C{})", self.line, self.column)
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        self.line == other.line && self.column == other.column
    }
}

impl Eq for Position {}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.bytes.cmp(&other.bytes))
    }
}

impl Add<&str> for Position {
    type Output = Position;
    fn add(self, rhs: &str) -> Self::Output {
        return Position {
            line: self.line,
            column: self.column + rhs.len() as i32,
            chars: self.chars + rhs.chars().count() as i32,
            bytes: self.bytes + rhs.bytes().len() as i32,
        };
    }
}

impl AddAssign<&str> for Position {
    fn add_assign(&mut self, rhs: &str) -> () {
        self.column = self.column + rhs.len() as i32;
        self.chars = self.chars + rhs.chars().count() as i32;
        self.bytes = self.bytes + rhs.bytes().len() as i32;
    }
}

impl Default for Position {
    fn default() -> Self {
        Position {
            line: 1,
            column: 1,
            chars: 0,
            bytes: 0,
        }
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.line, self.column)
    }
}
