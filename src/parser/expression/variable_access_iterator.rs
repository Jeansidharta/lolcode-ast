use crate::parser::expression::identifier_iterator::IdentifierTokensIterator;
use crate::parser::expression::VariableAccess;
use crate::parser::Token;

enum VariableAccessTokenIteratorState<'a> {
    Start,
    Identifier(IdentifierTokensIterator<'a>),
    Access((usize, Option<IdentifierTokensIterator<'a>>)),
    Done,
}

pub struct VariableAccessTokenIterator<'a> {
    variable_access: &'a VariableAccess,
    state: VariableAccessTokenIteratorState<'a>,
}

impl<'a> VariableAccessTokenIterator<'a> {
    pub fn new(variable_access: &'a VariableAccess) -> Self {
        VariableAccessTokenIterator {
            variable_access,
            state: VariableAccessTokenIteratorState::Start,
        }
    }
}

impl<'a> Iterator for VariableAccessTokenIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            VariableAccessTokenIteratorState::Start => {
                let ident_tokens = self.variable_access.identifier.tokens();
                self.state = VariableAccessTokenIteratorState::Identifier(ident_tokens);
                self.next()
            }
            VariableAccessTokenIteratorState::Identifier(ref mut ident_iterator) => {
                match ident_iterator.next() {
                    Some(val) => Some(val),
                    None => {
                        self.state = VariableAccessTokenIteratorState::Access((0, None));
                        self.next()
                    }
                }
            }
            VariableAccessTokenIteratorState::Access((index, ref mut ident_iter)) => {
                match ident_iter.as_mut().and_then(|t| t.next()) {
                    Some(token) => Some(token),
                    None => match self.variable_access.accesses.get(index) {
                        Some(ident) => {
                            self.state = VariableAccessTokenIteratorState::Access((
                                index + 1,
                                Some(ident.tokens()),
                            ));
                            self.next()
                        }
                        None => {
                            self.state = VariableAccessTokenIteratorState::Done;
                            None
                        }
                    },
                }
            }
            VariableAccessTokenIteratorState::Done => None,
        }
    }
}
