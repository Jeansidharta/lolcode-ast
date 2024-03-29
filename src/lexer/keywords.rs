#![allow(non_camel_case_types)]

use std::fmt::Debug;

use lolcode_ast_derive::{IterableEnum, ToStringSlice};

/// The enum with all LOLCODE keywords
///
/// "Keyword" in this case is not necessarily a single word, but one or more words that represent
/// an operation in LOLCODE, such as "I HAS A" or "ITZ".
/// All spaces beteween words are replaced by underscores
#[derive(ToStringSlice, IterableEnum, Clone, PartialEq, Debug)]
// No point in documenting all keywords here. Will just allow missing_docs
#[allow(missing_docs)]
pub enum Keywords {
    BTW,
    HAS_A,
    OBTW,
    SRS,
    TLDR,
    BUKKIT,
    HAI,
    KTHXBYE,
    I_HAS_A,
    ITZ,
    NOOB,
    TROOF,
    FAIL,
    WIN,
    NUMBR,
    NUMBAR,
    YARN,
    MKAY,
    AN,
    SUM_OF,
    DIFF_OF,
    PRODUKT_OF,
    QUOSHUNT_OF,
    MOD_OF,
    BIGGR_OF,
    SMALLR_OF,
    BOTH_SAEM,
    BOTH_OF,
    EITHER_OF,
    WON_OF,
    NOT,
    UPPIN,
    NERFIN,
    ALL_OF,
    ANY_OF,
    DIFFRINT,
    SMOOSH,
    MAEK,
    IS_NOW,
    VISIBLE,
    GIMMEH,
    O_RLY,
    YA_RLY,
    NO_WAI,
    OIC,
    MEBBE,
    WTF,
    OMGWTF,
    OMG,
    GTFO,
    IM_IN_YR,
    YR,
    TIL,
    WILE,
    IM_OUTTA_YR,
    HOW_IZ_I,
    IF_U_SAY_SO,
    FOUND_YR,
    I_IZ,
    R,
    A,
}

impl Keywords {
    /// Returns a &str for the keyword.
    ///
    /// ```rust
    /// # use lolcode_ast::lexer::Keywords;
    /// let keyword = Keywords::I_HAS_A;
    /// assert_eq!(keyword.into_str(), "I HAS A")
    /// ```
    pub fn to_string_slice(&self) -> &'static str {
        <Keywords as crate::to_string_slice::ToStringSlice>::to_string_slice(self)
    }
}
