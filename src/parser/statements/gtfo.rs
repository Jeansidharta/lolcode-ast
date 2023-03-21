use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Gtfo {
    gtfo_token: Token,
}

pub(crate) fn parse_gtfo(token: Token) -> Gtfo {
    Gtfo { gtfo_token: token }
}
