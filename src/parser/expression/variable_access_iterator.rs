use crate::parser::expression::identifier_iterator::IdentifierTokensIterator;
use crate::parser::expression::VariableAccess;
use crate::parser::Token;

use super::variable_access::VariableAccessSlot;

enum VariableAccessSlotIteratorState<'a> {
    Slot,
    Identifier(IdentifierTokensIterator<'a>),
    End,
}

pub struct VariableAccessSlotIterator<'a> {
    access_lot: &'a VariableAccessSlot,
    state: VariableAccessSlotIteratorState<'a>,
}

impl<'a> VariableAccessSlotIterator<'a> {
    pub(crate) fn new(access_lot: &'a VariableAccessSlot) -> Self {
        Self {
            access_lot,
            state: VariableAccessSlotIteratorState::Slot,
        }
    }
}

impl<'a> Iterator for VariableAccessSlotIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            VariableAccessSlotIteratorState::Slot => {
                self.state = VariableAccessSlotIteratorState::Identifier(
                    self.access_lot.identifier.tokens(),
                );
                Some(&self.access_lot.slot_access_token)
            }
            VariableAccessSlotIteratorState::Identifier(ref mut iter) => {
                iter.next().or_else(|| {
                    self.state = VariableAccessSlotIteratorState::End;
                    None
                })
            }
            VariableAccessSlotIteratorState::End => None,
        }
    }
}

enum VariableAccessTokenIteratorState<'a> {
    Start,
    Identifier(IdentifierTokensIterator<'a>),
    Access((usize, Option<VariableAccessSlotIterator<'a>>)),
    Done,
}

pub struct VariableAccessIterator<'a> {
    variable_access: &'a VariableAccess,
    state: VariableAccessTokenIteratorState<'a>,
}

impl<'a> VariableAccessIterator<'a> {
    pub fn new(variable_access: &'a VariableAccess) -> Self {
        VariableAccessIterator {
            variable_access,
            state: VariableAccessTokenIteratorState::Start,
        }
    }
}

impl<'a> Iterator for VariableAccessIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            VariableAccessTokenIteratorState::Start => {
                let ident_tokens = self.variable_access.identifier.tokens();
                self.state = VariableAccessTokenIteratorState::Identifier(ident_tokens);
                self.next()
            }
            VariableAccessTokenIteratorState::Identifier(ref mut ident_iterator) => {
                ident_iterator.next().or_else(|| {
                    self.state = VariableAccessTokenIteratorState::Access((0, None));
                    self.next()
                })
            }
            VariableAccessTokenIteratorState::Access((index, ref mut ident_iter)) => {
                match ident_iter.as_mut().and_then(|t| t.next()) {
                    Some(token) => Some(token),
                    None => match self.variable_access.accesses.get(index) {
                        Some(access_slot) => {
                            self.state = VariableAccessTokenIteratorState::Access((
                                index + 1,
                                Some(access_slot.tokens()),
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

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use crate::lexer::{Keywords, TokenType};

    use super::*;

    #[test]
    fn no_srs_no_access() {
        let name = Token::from(TokenType::Identifier("VAR".to_string()));
        let variable_access = VariableAccess::parse(name.clone(), &mut [].into()).unwrap();
        let mut iterator = variable_access.tokens();

        assert_eq!(iterator.next(), Some(&name));
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn no_srs_two_accesses() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Identifier("Var".to_string()),
                TokenType::BukkitSlotAccess,
                TokenType::Identifier("Access_1".to_string()),
                TokenType::BukkitSlotAccess,
                TokenType::Identifier("Access_2".to_string()),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();
        let variable_access =
            VariableAccess::parse(first_token.clone(), &mut tokens.clone().into()).unwrap();
        let mut iterator = variable_access.tokens();

        assert_eq!(iterator.next(), Some(&first_token));
        assert_eq!(iterator.next(), Some(&tokens[0]));
        assert_eq!(iterator.next(), Some(&tokens[1]));
        assert_eq!(iterator.next(), Some(&tokens[2]));
        assert_eq!(iterator.next(), Some(&tokens[3]));
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn some_srs_two_accesses() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Keyword(Keywords::SRS),
                TokenType::Identifier("Var".to_string()),
                TokenType::BukkitSlotAccess,
                TokenType::Keyword(Keywords::SRS),
                TokenType::Identifier("Access_1".to_string()),
                TokenType::BukkitSlotAccess,
                TokenType::Identifier("Access_2".to_string()),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();
        let variable_access =
            VariableAccess::parse(first_token.clone(), &mut tokens.clone().into()).unwrap();
        let mut iterator = variable_access.tokens();

        assert_eq!(iterator.next(), Some(&first_token));
        assert_eq!(iterator.next(), Some(&tokens[0]));
        assert_eq!(iterator.next(), Some(&tokens[1]));
        assert_eq!(iterator.next(), Some(&tokens[2]));
        assert_eq!(iterator.next(), Some(&tokens[3]));
        assert_eq!(iterator.next(), Some(&tokens[4]));
        assert_eq!(iterator.next(), Some(&tokens[5]));
        assert_eq!(iterator.next(), None);
    }
}
