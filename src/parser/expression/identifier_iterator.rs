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
        let identifier = Identifier::parse(name.clone(), &mut [].into()).unwrap();
        let mut iterator = identifier.tokens();

        assert_eq!(iterator.next(), Some(&name));
        assert_eq!(iterator.next(), None);
    }

    #[test]
    fn with_srs() {
        let mut tokens = Token::make_line(
            vec![
                TokenType::Keyword(Keywords::SRS),
                TokenType::Identifier("VAR".to_string()),
            ],
            0,
        );

        let first_token = tokens.pop_front().unwrap();

        let identifier =
            Identifier::parse(first_token.clone(), &mut tokens.clone().into()).unwrap();

        let mut iterator = identifier.tokens();

        assert_eq!(iterator.next(), Some(&first_token));
        assert_eq!(iterator.next(), Some(&tokens[0]));
        assert_eq!(iterator.next(), None);
    }
}
