use crate::parser::expression::Identifier;
use crate::parser::Token;

enum IdentifierTokensIteratorState {
    Start,
    HasUsedSrs,
    Done,
}

pub struct IdentifierTokensIterator<'a> {
    identifier: &'a Identifier,
    state: IdentifierTokensIteratorState,
}

impl<'a> IdentifierTokensIterator<'a> {
    pub(crate) fn new(identifier: &'a Identifier) -> IdentifierTokensIterator<'a> {
        IdentifierTokensIterator {
            identifier,
            state: IdentifierTokensIteratorState::Start,
        }
    }
}

impl<'a> Iterator for IdentifierTokensIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            IdentifierTokensIteratorState::Start => match self.identifier.srs {
                Some(ref srs) => {
                    self.state = IdentifierTokensIteratorState::HasUsedSrs;
                    Some(&srs)
                }
                None => {
                    self.state = IdentifierTokensIteratorState::Done;
                    Some(&self.identifier.name)
                }
            },
            IdentifierTokensIteratorState::HasUsedSrs => {
                self.state = IdentifierTokensIteratorState::Done;
                Some(&self.identifier.name)
            }
            IdentifierTokensIteratorState::Done => None,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{Keywords, TokenType};
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn no_srs() {
        let name = Token::from(TokenType::Identifier("VAR".to_string()));
        let identifier = Identifier {
            srs: None,
            name: name.clone(),
        };
        let mut iterator = identifier.tokens();

        assert_eq!(iterator.next(), Some(&name));
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn with_srs() {
        let srs = Token::from(TokenType::Keyword(Keywords::SRS));
        let name = Token::from(TokenType::Identifier("VAR".to_string()));

        let identifier = Identifier {
            srs: Some(srs.clone()),
            name: name.clone(),
        };
        let mut iterator = identifier.tokens();

        assert_eq!(iterator.next(), Some(&srs));
        assert_eq!(iterator.next(), Some(&name));
        assert_eq!(iterator.next(), None);
    }
}
