use self::assignment::VariableAssignment;
use self::bukkit_set_slot::BukkitSetSlot;
use self::hai::Hai;
use self::how_is_i::HowIzI;
use self::i_has_a::IHasA;
use self::i_is::IIz;
use self::im_in_yr::ImInYr;
use self::o_rly::ORly;
use self::visible::Visible;
use self::wtf::Wtf;

use super::expression::parse_expression;
use super::expression::variable_access::{is_valid_variable_access, parse_variable_access};
use super::types::{ASTErrorType, ASTExpression, ASTNode};
use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::StatementIterator;

pub mod assignment;
pub mod bukkit_set_slot;
pub mod found_yr;
pub mod gimmeh;
pub mod gtfo;
pub mod hai;
pub mod how_is_i;
pub mod i_has_a;
pub mod i_is;
pub mod im_in_yr;
pub mod kthxbye;
pub mod o_rly;
pub mod visible;
pub mod wtf;

pub fn parse_statement(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTNode, ASTErrorType> {
    if is_valid_variable_access(&first_token) {
        let variable_access = parse_variable_access(first_token, tokens)?;
        return match tokens.peek().map(|t| &t.token_type) {
            None => Ok(ASTNode::Expression(ASTExpression::VariableAccess(
                variable_access,
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
        TokenType::Keyword(Keywords::GIMMEH) => gimmeh::parse_gimmeh(first_token, tokens),
        TokenType::Keyword(Keywords::FOUND_YR) => found_yr::parse_found_yr(first_token, tokens),
        TokenType::Keyword(Keywords::HAI) => Hai::try_from(tokens).map(|t| t.into()),
        TokenType::Keyword(Keywords::KTHXBYE) => kthxbye::parse_kthxbye(first_token),
        TokenType::Keyword(Keywords::GTFO) => gtfo::parse_gtfo(first_token),
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
        _ => parse_expression(first_token, tokens).map(|e| ASTNode::Expression(e)),
    }
}
