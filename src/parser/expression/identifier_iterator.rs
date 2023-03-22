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
