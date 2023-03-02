use std::collections::VecDeque;

use crate::lexer::Token;
use crate::parser::expression::ExpressionError;
use crate::parser::statements::assignment::VariableAssignmentError;
use crate::parser::statements::how_is_i::HowIsIError;
use crate::parser::statements::i_has_a::IHasAError;
use crate::parser::statements::i_is::IIzError;
use crate::parser::statements::im_in_yr::ImInYrError;
use crate::parser::statements::o_rly::ORlyError;

/// General error that can happen when parsing statements in general
#[derive(Debug, PartialEq, Clone)]
pub enum ASTErrorType {
    /// The given token was not expected in the current parsing context
    UnexpectedToken(Token),
    /// If these tokens were not there, the statement would be valid
    TooManyTokens(VecDeque<Token>),
    /// `I HAS A` expression specific errors
    IHasA(IHasAError),
    /// Variable Assignment specific errors
    VariableAssignmentError(VariableAssignmentError),
    /// Expression specific errors
    Expression(ExpressionError),
    /// A token was expected here, but nothing was found
    MissingToken(Token),
    /// `O RLY?` specific errors
    ORlyError(ORlyError),
    /// A previously opened block has no closing token
    MissingBlockClosingToken(Token),
    /// `HOW IS I` specific errors
    HowIsIError(HowIsIError),
    /// `IM IN YR` specific errors
    ImInYr(ImInYrError),
    /// `I IZ` specific errors
    IIz(IIzError),
}
