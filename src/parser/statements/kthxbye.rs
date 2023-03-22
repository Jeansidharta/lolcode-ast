use crate::lexer::Token;

/// Represents a KTHXBYE statement. This ends the program.
#[derive(Debug, PartialEq, Clone)]
pub struct KThxBye {
    k_thx_bye_token: Token,
}

pub(crate) fn parse_kthxbye(first_token: Token) -> KThxBye {
    KThxBye {
        k_thx_bye_token: first_token,
    }
}
