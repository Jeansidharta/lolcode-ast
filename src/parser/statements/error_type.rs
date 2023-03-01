use std::collections::VecDeque;

use crate::lexer::Token;
use crate::parser::expression::ExpressionError;
use crate::parser::statements::assignment::VariableAssignmentError;
use crate::parser::statements::how_is_i::HowIsIError;
use crate::parser::statements::i_has_a::IHasAError;
use crate::parser::statements::i_is::IIzError;
use crate::parser::statements::im_in_yr::ImInYrError;
use crate::parser::statements::o_rly::ORlyError;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTErrorType {
    UnexpectedToken(Token),
    TooManyTokens(VecDeque<Token>),
    IHasA(IHasAError),
    VariableAssignmentError(VariableAssignmentError),
    Expression(ExpressionError),
    MissingToken(Token),
    ORlyError(ORlyError),
    MissingBlockClosingToken(Token),
    HowIsIError(HowIsIError),
    ImInYr(ImInYrError),
    IIz(IIzError),
}
