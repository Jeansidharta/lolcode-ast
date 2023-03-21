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
use self::o_rly::ORly;
use self::visible::Visible;
use self::wtf::Wtf;
use crate::parser::error::ASTErrorType;

use crate::parser::expression::parse_expression;

use super::expression::variable_access::{is_valid_variable_access, parse_variable_access};
use super::expression::{ASTExpression, ASTExpressionValue};
use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::expression::VariableAccess;
use crate::parser::StatementIterator;

/// Varaible assignemnt statement. Ex: `var R "some_value"`
pub mod assignment;
/// Assign a value to a bukkit's slot. Ex: `var HAS A slot_name ITZ "some_value"`
pub mod bukkit_set_slot;
pub(crate) mod error_type;
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
    HAI(Hai),
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
    KTHXBYE(Token),
}

impl<T: Into<Node>> From<Result<T, ASTErrorType>> for Node {
    fn from(value: Result<T, ASTErrorType>) -> Self {
        match value {
            Err(err) => Node::ASTError(err),
            Ok(val) => val.into(),
        }
    }
}

impl From<ASTErrorType> for Node {
    fn from(value: ASTErrorType) -> Self {
        Node::ASTError(value)
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
                VariableAssignment::try_from((variable_access, tokens)).map(|t| t.into())
            }
            Some(TokenType::Keyword(Keywords::HAS_A)) => {
                BukkitSetSlot::try_from((variable_access, tokens)).map(|t| t.into())
            }

            // TODO - fix this mess
            Some(_) => unreachable!(),
        };
    }

    match first_token.token_type {
        TokenType::Keyword(Keywords::I_HAS_A) => {
            IHasA::try_from((first_token, tokens)).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::VISIBLE) => Visible::try_from(tokens).map(|t| t.into()),
        TokenType::Keyword(Keywords::GIMMEH) => {
            gimmeh::parse_gimmeh(first_token, tokens).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::FOUND_YR) => {
            found_yr::parse_found_yr(first_token, tokens).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::HAI) => Hai::try_from(tokens).map(|t| t.into()),
        TokenType::Keyword(Keywords::KTHXBYE) => kthxbye::parse_kthxbye(first_token),
        TokenType::Keyword(Keywords::GTFO) => gtfo::parse_gtfo(first_token).into(),
        TokenType::Keyword(Keywords::I_IZ) => {
            IIz::try_from((first_token, tokens)).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::IM_IN_YR) => {
            ImInYr::try_from((first_token, tokens)).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::HOW_IZ_I) => {
            HowIzI::try_from((first_token, tokens)).map(|t| t.into())
        }
        TokenType::Keyword(Keywords::WTF) => Wtf::try_from((first_token, tokens)).map(|t| t.into()),
        TokenType::Keyword(Keywords::O_RLY) => {
            ORly::try_from((first_token, tokens)).map(|t| t.into())
        }
        // Try to parse it as an expression
        _ => parse_expression(first_token, tokens).map(|e| Node::Expression(e)),
    }
}
