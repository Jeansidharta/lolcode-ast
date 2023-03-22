use crate::lexer::Token;

/// Represents a `GTFO` statement. It returns `NOOB` from a function.
/// It is equivalent to `FOUND YR NOOB`
///
/// ```LOLCode
/// HOW IZ I is_slot_even YR bukkit AN YR slot
///     I HAS A var ITZ bukkit'Z SRS slot
///
///     BOTH SAEM var AN NOOB, O RLY?
///         YA RLY, GTFO    BTW returns early from the function
///     OIC
///
///     MOD OF var AN 2, O RLY?
///         YA RLY, FOUND YR "ODD"
///         NO WAI, FOUND YR "EVEN"
///     OIC
/// IF U SAY SO
/// ```
#[derive(Debug, PartialEq, Clone)]
pub struct Gtfo {
    gtfo_token: Token,
}

impl Gtfo {
    pub(crate) fn parse(token: Token) -> Gtfo {
        Gtfo { gtfo_token: token }
    }
}
