use crate::lexer::Position;
use std::fmt::Debug;

/// A Range is a section of the text with a start position and an end position.
#[derive(Clone)]
pub struct Range(pub Position, pub Position);

impl From<(Position, Position)> for Range {
    fn from((start, end): (Position, Position)) -> Self {
        Range(start, end)
    }
}

impl From<Position> for Range {
    fn from(value: Position) -> Self {
        Range(value.clone(), value)
    }
}

impl Debug for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?} -> {:?}", self.0, self.1)
    }
}

impl PartialEq for Range {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl Range {
    pub(crate) fn start_line(&self) -> i32 {
        self.0.line
    }
}

#[cfg(test)]
pub mod tests {
    use std::ops::Add;

    use super::Range;

    impl Add<&str> for Range {
        type Output = Range;
        fn add(mut self, rhs: &str) -> Self::Output {
            self.1.column += rhs.len() as i32;
            self
        }
    }

    impl Range {
        pub fn after(&self) -> Range {
            let mut start = self.1.clone();
            start.column += 1;
            Range(start.clone(), start)
        }
        pub fn set_line(&mut self, new_line: i32) {
            self.0.line = new_line;
            self.1.line = new_line;
        }
    }
}
