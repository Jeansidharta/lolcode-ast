use std::collections::VecDeque;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::lexer::Token;

use super::statements::assignment::{VariableAssignment, VariableAssignmentError};
use super::statements::bukkit_set_slot::BukkitSetSlot;
use super::statements::hai::Hai;
use super::statements::how_is_i::{HowIsIError, HowIzI};
use super::statements::i_has_a::{IHasA, IHasAError};
use super::statements::i_is::{IIz, IIzError};
use super::statements::im_in_yr::{ImInYr, ImInYrError};
use super::statements::o_rly::{ORly, ORlyError};
use super::statements::visible::Visible;
use super::statements::wtf::Wtf;

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionError {
    MissingOperands(Token),
}

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

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub name: Token,
    pub is_srs: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableAccess {
    pub name: Identifier,
    pub accesses: VecDeque<Identifier>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpression {
    LiteralValue(Token),
    VariableAccess(VariableAccess),
    BothOf(Rc<ASTExpression>, Rc<ASTExpression>),
    EitherOf(Rc<ASTExpression>, Rc<ASTExpression>),
    WonOf(Rc<ASTExpression>, Rc<ASTExpression>),
    Not(Rc<ASTExpression>),
    AllOf(VecDeque<ASTExpression>),
    AnyOf(VecDeque<ASTExpression>),
    SumOf(Rc<ASTExpression>, Rc<ASTExpression>),
    DiffOf(Rc<ASTExpression>, Rc<ASTExpression>),
    ProduktOf(Rc<ASTExpression>, Rc<ASTExpression>),
    QuoshuntOf(Rc<ASTExpression>, Rc<ASTExpression>),
    ModOf(Rc<ASTExpression>, Rc<ASTExpression>),
    BiggrOf(Rc<ASTExpression>, Rc<ASTExpression>),
    SmallrOf(Rc<ASTExpression>, Rc<ASTExpression>),
    BothSaem(Rc<ASTExpression>, Rc<ASTExpression>),
    Diffrint(Rc<ASTExpression>, Rc<ASTExpression>),
    Smoosh(VecDeque<ASTExpression>),
    Maek(Rc<ASTExpression>, Token),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTType {
    Bukkit,
    Numbr,
    Numbar,
    Yarn,
    Troof,
    Noob,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    IHasA(IHasA),
    HAI(Hai),
    ImInYr(ImInYr),
    BukkitSetSlot(BukkitSetSlot),
    VariableAssignment(VariableAssignment),
    Visible(Visible),
    FoundYr(ASTExpression),
    Wtf(Wtf),
    ORly(ORly),
    IIz(IIz),
    HowIzI(HowIzI),
    Gtfo(Token),
    Gimmeh(VariableAccess),
    Expression(ASTExpression),
    ASTError(ASTErrorType),
    KTHXBYE(Token),
}

impl<T: Into<ASTNode>> From<Result<T, ASTErrorType>> for ASTNode {
    fn from(value: Result<T, ASTErrorType>) -> Self {
        match value {
            Err(err) => ASTNode::ASTError(err),
            Ok(val) => val.into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ASTBlock(VecDeque<ASTNode>);

impl ASTBlock {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }
}

impl<const T: usize> From<[ASTNode; T]> for ASTBlock {
    fn from(value: [ASTNode; T]) -> Self {
        Self(VecDeque::from(value))
    }
}

impl Deref for ASTBlock {
    type Target = VecDeque<ASTNode>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ASTBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl VariableAccess {
    pub fn last_token(mut self) -> Token {
        self.accesses
            .pop_back()
            .map(|ident| ident.name)
            .unwrap_or_else(|| self.name.name)
    }
}

impl<const T: usize> From<((Token, bool), [(Token, bool); T])> for VariableAccess {
    fn from((name, accesses): ((Token, bool), [(Token, bool); T])) -> Self {
        VariableAccess {
            name: name.into(),
            accesses: accesses.into_iter().map(|a| a.into()).collect(),
        }
    }
}

impl From<(Token, bool)> for Identifier {
    fn from((name, is_srs): (Token, bool)) -> Self {
        Self { name, is_srs }
    }
}

impl From<ASTErrorType> for ASTNode {
    fn from(value: ASTErrorType) -> Self {
        ASTNode::ASTError(value)
    }
}

#[cfg(test)]
pub mod test {

    use super::ASTType;
    use crate::lexer::Keywords;

    impl TryFrom<&Keywords> for ASTType {
        type Error = ();
        fn try_from(value: &Keywords) -> Result<Self, ()> {
            match value {
                Keywords::NUMBAR => Ok(Self::Numbar),
                Keywords::NUMBR => Ok(Self::Numbr),
                Keywords::TROOF => Ok(Self::Troof),
                Keywords::YARN => Ok(Self::Yarn),
                Keywords::NOOB => Ok(Self::Noob),
                Keywords::BUKKIT => Ok(Self::Bukkit),
                _ => Err(()),
            }
        }
    }
}
