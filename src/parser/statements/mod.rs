use self::assignment::VariableAssignment;
use self::bukkit_set_slot::BukkitSetSlot;
use self::found_yr::FoundYr;
use self::gimmeh::Gimmeh;
use self::gtfo::Gtfo;
use self::hai::Hai;
use self::how_is_i::HowIzI;
use self::i_has_a::IHasA;
use self::i_is::IIz;
use self::im_in_yr::ImInYr;
use self::kthxbye::KThxBye;
use self::o_rly::ORly;
use self::visible::Visible;
use self::wtf::Wtf;
use crate::parser::error::ASTErrorType;

use super::expression::variable_access::{is_valid_variable_access, parse_variable_access};
use super::expression::{ASTExpression, ASTExpressionValue};
use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::expression::VariableAccess;
use crate::parser::StatementIterator;

/// Varaible assignemnt statement. Ex: `var R "some_value"`
pub mod assignment;
/// Assign a value to a bukkit's slot. Ex: `var HAS A slot_name ITZ "some_value"`
pub mod bukkit_set_slot;
/// Returs a value from within a function. Ex: `FOUND YR "some_value"`
pub mod found_yr;
/// Reads user input into a variable. Ex: `GIMMEH var`
pub mod gimmeh;
/// Returns `NOOB` from within a function. Ex: `GTFO`
pub mod gtfo;
/// Initializes a program, specifying it's LOLCODE version. Ex: `HAI 1.3`
pub mod hai;
/// Defines a function.
///
/// Ex:
/// ```LOLCODE
/// HOW IZ I function_name YR argument1 AN YR argument2
///     BTW some code here
/// IF U SAY SO
/// ```
pub mod how_is_i;
/// Variable Initialization. Ex: `I HAS A var ITZ "some_value"`
pub mod i_has_a;
/// Calls a function. Ex: `I IZ some_function YR "value 1" AN YR "value 2"`
pub mod i_is;
/// Loop
///
/// Ex:
/// ```LOLCODE
/// IM IN YR loop UPPIN YR var TIL BOTH SAEM var AN 10
///     VISIBLE var
/// IM OUTTA YR loop
/// ```
pub mod im_in_yr;
/// Ends the program. This should be the last statement of the program.
pub mod kthxbye;
/// This is somewhat of an If statement.
///
/// Ex:
/// ```LOLCODE
/// BOTH SAEM var AN 0, O RLY?
///     YA RLY
///         VISIBLE "var is 0"
///     MEBBE BOTH SAEM var AN 1
///         VISIBLE "var is 1"
///     NO WAI
///         VISIBLE "var is neither 0 or 1"
/// OIC
/// ```
pub mod o_rly;
/// Prints the given statements to stdout
pub mod visible;
/// A switch statement
///
/// Ex:
/// ```LOLCODE
/// var, WTF?
///     OMG 0
///         VISIBLE "var is 0"
///     OMG 1
///         VISIBLE "var is 1"
///     OMGWTF
///         VISIBLE "var is neither 0 or 1"
/// OIC
/// ```
pub mod wtf;

/// An AST node that represents a LOLCODE statement.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    IHasA(IHasA),
    Hai(Hai),
    ImInYr(ImInYr),
    BukkitSetSlot(BukkitSetSlot),
    VariableAssignment(VariableAssignment),
    Visible(Visible),
    FoundYr(FoundYr),
    Wtf(Wtf),
    ORly(ORly),
    IIz(IIz),
    HowIzI(HowIzI),
    Gtfo(Gtfo),
    Gimmeh(Gimmeh),
    Expression(ASTExpression),
    ASTError(ASTErrorType),
    KThxBye(KThxBye),
}

impl From<IHasA> for Node {
    fn from(value: IHasA) -> Node {
        Node::IHasA(value)
    }
}
impl From<Hai> for Node {
    fn from(value: Hai) -> Node {
        Node::Hai(value)
    }
}
impl From<ImInYr> for Node {
    fn from(value: ImInYr) -> Node {
        Node::ImInYr(value)
    }
}
impl From<BukkitSetSlot> for Node {
    fn from(value: BukkitSetSlot) -> Node {
        Node::BukkitSetSlot(value)
    }
}
impl From<VariableAssignment> for Node {
    fn from(value: VariableAssignment) -> Node {
        Node::VariableAssignment(value)
    }
}
impl From<Visible> for Node {
    fn from(value: Visible) -> Node {
        Node::Visible(value)
    }
}
impl From<FoundYr> for Node {
    fn from(value: FoundYr) -> Node {
        Node::FoundYr(value)
    }
}
impl From<Wtf> for Node {
    fn from(value: Wtf) -> Node {
        Node::Wtf(value)
    }
}
impl From<ORly> for Node {
    fn from(value: ORly) -> Node {
        Node::ORly(value)
    }
}
impl From<IIz> for Node {
    fn from(value: IIz) -> Node {
        Node::IIz(value)
    }
}
impl From<HowIzI> for Node {
    fn from(value: HowIzI) -> Node {
        Node::HowIzI(value)
    }
}
impl From<Gtfo> for Node {
    fn from(value: Gtfo) -> Node {
        Node::Gtfo(value)
    }
}
impl From<Gimmeh> for Node {
    fn from(value: Gimmeh) -> Node {
        Node::Gimmeh(value)
    }
}

impl From<ASTExpression> for Node {
    fn from(value: ASTExpression) -> Node {
        Node::Expression(value)
    }
}
impl From<ASTErrorType> for Node {
    fn from(value: ASTErrorType) -> Node {
        Node::ASTError(value)
    }
}
impl From<KThxBye> for Node {
    fn from(value: KThxBye) -> Node {
        Node::KThxBye(value)
    }
}

impl<T: Into<Node>> From<Result<T, ASTErrorType>> for Node {
    fn from(value: Result<T, ASTErrorType>) -> Self {
        match value {
            Err(err) => Node::ASTError(err),
            Ok(val) => val.into(),
        }
    }
}

pub(crate) fn parse_statement(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<Node, ASTErrorType> {
    if is_valid_variable_access(&first_token) {
        let variable_access = parse_variable_access(first_token, tokens)?;
        return match tokens.peek().map(|t| &t.token_type) {
            None => Ok(Node::Expression(ASTExpression::Value(
                ASTExpressionValue::VariableAccess(variable_access),
            ))),
            Some(TokenType::Keyword(Keywords::R)) => {
                VariableAssignment::parse(variable_access, tokens).map(|t| t.into())
            }
            Some(TokenType::Keyword(Keywords::HAS_A)) => {
                BukkitSetSlot::parse(variable_access, tokens).map(|t| t.into())
            }

            // TODO - fix this mess
            Some(_) => unreachable!(),
        };
    }

    match first_token.token_type {
        TokenType::Keyword(Keywords::I_HAS_A) => {
            IHasA::parse(first_token, tokens).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::VISIBLE) => {
            Visible::parse(first_token, tokens).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::GIMMEH) => {
            gimmeh::Gimmeh::parse(first_token, tokens).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::FOUND_YR) => {
            found_yr::FoundYr::parse(first_token, tokens).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::HAI) => Hai::parse(first_token, tokens).map(|t| t.into()),
        TokenType::Keyword(Keywords::KTHXBYE) => Ok(kthxbye::KThxBye::parse(first_token).into()),
        TokenType::Keyword(Keywords::GTFO) => Ok(gtfo::Gtfo::parse(first_token).into()),
        TokenType::Keyword(Keywords::I_IZ) => IIz::parse(first_token, tokens).map(|t| t.into()),
        TokenType::Keyword(Keywords::IM_IN_YR) => {
            ImInYr::parse(first_token, tokens).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::HOW_IZ_I) => {
            HowIzI::parse(first_token, tokens).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::WTF) => Wtf::parse(first_token, tokens).map(|t| t.into()),
        TokenType::Keyword(Keywords::O_RLY) => ORly::parse(first_token, tokens).map(|t| t.into()),
        // Try to parse it as an expression
        _ => ASTExpression::parse(first_token, tokens).map(|e| Node::Expression(e)),
    }
}
