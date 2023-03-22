use crate::lexer::{Keywords, Position, Token, TokenType};
use crate::parser::error::ASTErrorType;
use crate::parser::StatementIterator;

pub(crate) mod identifier;
pub(crate) mod variable_access;

pub use identifier::{ASTType, Identifier};
pub use variable_access::VariableAccess;

/// All binary operators. Each variant represents a different operation. The associated token is
/// a keyword token that generated the operator.
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOpt {
    /// Checks if left or right are true (left || right)
    /// ```LOLCode
    /// EITHER OF left AN right
    /// ```
    EitherOf(Token),
    /// A XOR between the left operant and the right operand. Will be true if left is different.
    /// than right.
    /// ```LOLCode
    /// WON OF left AN right
    /// ```
    WonOf(Token),
    /// Adds both operands as a number (left + right).
    /// ```LOLCode
    /// SUM OF left AN right
    /// ```
    SumOf(Token),
    /// Subtract right from left (left - right).
    /// ```LOLCode
    /// DIFF OF left AN right
    /// ```
    DiffOf(Token),
    /// Multiplies left and right (left * right).
    /// ```LOLCode
    /// DIFF OF left AN right
    /// ```
    ProduktOf(Token),
    /// Divides left and right (left / right).
    /// ```LOLCode
    /// QUOSHUNT OF left AN right
    /// ```
    QuoshuntOf(Token),
    /// Remainder of a division between left and right (left % right).
    /// ```LOLCode
    /// MOD OF left AN right
    /// ```
    ModOf(Token),
    /// Checks if left is bigger than right (left > right).
    /// ```LOLCode
    /// BIGGR OF left AN right
    /// ```
    BiggrOf(Token),
    /// Checks of left is smaller than right (left < right).
    /// ```LOLCode
    /// SMALLR OF left AN right
    /// ```
    SmallrOf(Token),
    /// Checks if left is equal to right (left == right)
    /// ```LOLCode
    /// BOTH SAME left AN right
    /// ```
    BothSaem(Token),
    /// Checks if left and right are both true (left && right)
    /// ```LOLCode
    /// BOTH OF left AN right
    /// ```
    BothOf(Token),
    /// Checks if left is different from right (left != right)
    /// ```LOLCode
    /// DIFFRINT left AN right
    /// ```
    Diffrint(Token),
}

impl BinaryOpt {
    /// Gets a reference to the token inside the operator
    pub fn token(&self) -> &Token {
        match self {
            BinaryOpt::EitherOf(token) => token,
            BinaryOpt::WonOf(token) => token,
            BinaryOpt::SumOf(token) => token,
            BinaryOpt::DiffOf(token) => token,
            BinaryOpt::ProduktOf(token) => token,
            BinaryOpt::QuoshuntOf(token) => token,
            BinaryOpt::ModOf(token) => token,
            BinaryOpt::BiggrOf(token) => token,
            BinaryOpt::SmallrOf(token) => token,
            BinaryOpt::BothSaem(token) => token,
            BinaryOpt::BothOf(token) => token,
            BinaryOpt::Diffrint(token) => token,
        }
    }

    /// Extracts the token from inside the operator, consuming the operator
    pub fn into_token(self) -> Token {
        match self {
            BinaryOpt::EitherOf(token) => token,
            BinaryOpt::WonOf(token) => token,
            BinaryOpt::SumOf(token) => token,
            BinaryOpt::DiffOf(token) => token,
            BinaryOpt::ProduktOf(token) => token,
            BinaryOpt::QuoshuntOf(token) => token,
            BinaryOpt::ModOf(token) => token,
            BinaryOpt::BiggrOf(token) => token,
            BinaryOpt::SmallrOf(token) => token,
            BinaryOpt::BothSaem(token) => token,
            BinaryOpt::BothOf(token) => token,
            BinaryOpt::Diffrint(token) => token,
        }
    }
}

/// All unary operators. Each variant represents a different operation. The associated token is
/// a keyword token that generated the operator.
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOpt {
    /// Converts the operand to a boolean, and then negates it.
    /// ```LOLCode
    /// NOT operand
    /// ```
    Not(Token),
}

impl UnaryOpt {
    /// Extracts the token from inside the operator, consuming the operator
    pub fn into_token(self) -> Token {
        match self {
            UnaryOpt::Not(token) => token,
        }
    }

    /// Gets a reference to the token inside the operator
    pub fn token(&self) -> &Token {
        match self {
            UnaryOpt::Not(token) => token,
        }
    }
}

/// All operators that can have any number of operands. Each variant represents a different operation. The associated token is
/// a keyword token that generated the operator.
#[derive(Debug, PartialEq, Clone)]
pub enum NaryOpt {
    /// Checks if all operands are true. (A && B && C && ...)
    /// ```LOLCode
    /// ALL OF A AN B AN C MKAY
    /// ```
    AllOf(Token),
    /// Concatenates all string operands
    /// ```LOLCode
    /// SMOOSH A AN B AN C MKAY
    /// ```
    Smoosh(Token),
    /// Checks if any operands are true. (A || B || C || ...)
    /// ```LOLCode
    /// ANY OF A AN B AN C MKAY
    /// ```
    AnyOf(Token),
}

impl NaryOpt {
    /// Extracts the token from inside the operator, consuming the operator
    pub fn into_token(self) -> Token {
        match self {
            NaryOpt::AllOf(token) => token,
            NaryOpt::Smoosh(token) => token,
            NaryOpt::AnyOf(token) => token,
        }
    }

    /// Gets a reference to the token inside the operator
    pub fn token(&self) -> &Token {
        match self {
            NaryOpt::AllOf(token) => token,
            NaryOpt::Smoosh(token) => token,
            NaryOpt::AnyOf(token) => token,
        }
    }
}

/// The possible values when an expression is a value, not an operation.
#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpressionValue {
    /// The expression is a variable access. It can be either just referencing any variable, or
    /// accessing a bukkit's internal values.
    VariableAccess(VariableAccess),
    /// The expression is just one of the literal values, such as numbers, strings or booleans.
    LiteralValue(Token),
}

impl ASTExpressionValue {
    /// Gets the range of the entire expression
    pub fn range(&self) -> (&Position, &Position) {
        match self {
            ASTExpressionValue::VariableAccess(variable_access) => variable_access.range(),
            ASTExpressionValue::LiteralValue(token) => (&token.range.0, &token.range.1),
        }
    }
}

impl From<VariableAccess> for ASTExpressionValue {
    fn from(value: VariableAccess) -> Self {
        ASTExpressionValue::VariableAccess(value)
    }
}

impl TryFrom<Token> for ASTExpressionValue {
    type Error = ();
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.token_type {
            TokenType::Value(_) => Ok(ASTExpressionValue::LiteralValue(value)),
            _ => Err(()),
        }
    }
}

/// A operations with exactly two operands
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOperation {
    pub(crate) left: Box<ASTExpression>,
    pub(crate) operator: BinaryOpt,
    pub(crate) right: Box<ASTExpression>,
    pub(crate) an_token: Option<Token>,
}

/// A operation with exactly one operand
#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOperation {
    pub(crate) operator: UnaryOpt,
    pub(crate) expression: Box<ASTExpression>,
}

/// A operation with any number of operands
#[derive(Debug, PartialEq, Clone)]
pub struct NaryOperation {
    pub(crate) operator: NaryOpt,
    pub(crate) expressions: Vec<(ASTExpression, Option<Token>)>,
    pub(crate) mkay_token: Option<Token>,
}

/// A LOLCODE expression.
#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpression {
    /// The expression is an operation with exactly two operands
    BinaryOperation(BinaryOperation),
    /// The expression is an operation with exactly one operands
    UnaryOperation(UnaryOperation),
    /// The expression is an operation with any number of operands
    NaryOperation(NaryOperation),
    /// A operation with exactly any number of operands
    Value(ASTExpressionValue),
}

impl From<BinaryOperation> for ASTExpression {
    fn from(value: BinaryOperation) -> Self {
        ASTExpression::BinaryOperation(value)
    }
}

impl From<UnaryOperation> for ASTExpression {
    fn from(value: UnaryOperation) -> Self {
        ASTExpression::UnaryOperation(value)
    }
}

impl From<NaryOperation> for ASTExpression {
    fn from(value: NaryOperation) -> Self {
        ASTExpression::NaryOperation(value)
    }
}

impl ASTExpression {
    /// Gets the range (start position and end position) of the entire expression
    pub fn range(&self) -> (&Position, &Position) {
        match self {
            ASTExpression::BinaryOperation(BinaryOperation {
                operator, right, ..
            }) => (&operator.token().range.0, &right.range().1),
            ASTExpression::UnaryOperation(UnaryOperation {
                operator,
                expression,
            }) => (&operator.token().range.0, &expression.range().1),
            ASTExpression::NaryOperation(NaryOperation {
                operator,
                expressions,
                mkay_token,
            }) => (
                &operator.token().range.0,
                mkay_token
                    .as_ref()
                    .map(|t| &t.range.1)
                    .or_else(|| {
                        expressions
                            .last()
                            .map(|(_, t)| t.as_ref().map(|t| &t.range.1))
                            .flatten()
                    })
                    .or_else(|| expressions.last().map(|(expr, _)| expr.range().1))
                    .unwrap_or_else(|| &operator.token().range.1),
            ),
            ASTExpression::Value(value) => value.range(),
        }
    }
}

/// Errors that can only occur inside an expression
#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionError {
    /// Not enough operands were given for the operation
    MissingOperands(Token),
}

trait SwappableResult<T, U> {
    fn swap_ok_err(self) -> Result<U, T>;
}

impl<T, U> SwappableResult<T, U> for Result<T, U> {
    fn swap_ok_err(self) -> Result<U, T> {
        match self {
            Ok(ok) => Err(ok),
            Err(err) => Ok(err),
        }
    }
}

pub(crate) fn parse_expression(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    match first_token.token_type {
        TokenType::Identifier(_) | TokenType::Keyword(Keywords::SRS) => {
            variable_access::parse_variable_access(first_token, tokens)
                .map(|v| ASTExpression::Value(v.into()))
        }
        TokenType::Value(_) => Ok(ASTExpression::Value(ASTExpressionValue::LiteralValue(
            first_token,
        ))),
        TokenType::Keyword(Keywords::NOOB) => Ok(ASTExpression::Value(
            ASTExpressionValue::LiteralValue(first_token),
        )),
        TokenType::Keyword(ref keyword) => {
            match keyword {
                // Boolean operations
                Keywords::BOTH_OF => {
                    parse_binary_expression(BinaryOpt::BothOf(first_token), tokens)
                }
                Keywords::EITHER_OF => {
                    parse_binary_expression(BinaryOpt::EitherOf(first_token), tokens)
                }
                Keywords::WON_OF => parse_binary_expression(BinaryOpt::WonOf(first_token), tokens),
                Keywords::NOT => parse_unary_expression(UnaryOpt::Not(first_token), tokens),
                Keywords::ALL_OF => parse_nary_expression(NaryOpt::AllOf(first_token), tokens),
                Keywords::ANY_OF => parse_nary_expression(NaryOpt::AnyOf(first_token), tokens),
                // Math operations
                Keywords::SUM_OF => parse_binary_expression(BinaryOpt::SumOf(first_token), tokens),
                Keywords::DIFF_OF => {
                    parse_binary_expression(BinaryOpt::DiffOf(first_token), tokens)
                }
                Keywords::PRODUKT_OF => {
                    parse_binary_expression(BinaryOpt::ProduktOf(first_token), tokens)
                }
                Keywords::QUOSHUNT_OF => {
                    parse_binary_expression(BinaryOpt::QuoshuntOf(first_token), tokens)
                }
                Keywords::MOD_OF => parse_binary_expression(BinaryOpt::ModOf(first_token), tokens),
                // Comparison operations
                Keywords::BIGGR_OF => {
                    parse_binary_expression(BinaryOpt::BiggrOf(first_token), tokens)
                }
                Keywords::SMALLR_OF => {
                    parse_binary_expression(BinaryOpt::SmallrOf(first_token), tokens)
                }
                Keywords::BOTH_SAEM => {
                    parse_binary_expression(BinaryOpt::BothSaem(first_token), tokens)
                }
                Keywords::DIFFRINT => {
                    parse_binary_expression(BinaryOpt::Diffrint(first_token), tokens)
                }
                // String concat operation
                Keywords::SMOOSH => parse_nary_expression(NaryOpt::Smoosh(first_token), tokens),
                // Type Cast operation
                Keywords::MAEK => todo!(),

                _ => Err(ASTErrorType::UnexpectedToken(first_token)),
            }
        }
        _ => Err(ASTErrorType::UnexpectedToken(first_token)),
    }
}

fn parse_binary_expression(
    operator: BinaryOpt,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let left_first_token = match tokens.next() {
        None => {
            return Err(ASTErrorType::from(ExpressionError::MissingOperands(
                operator.into_token(),
            )))
        }
        Some(token) => token,
    };
    let left = parse_expression(left_first_token, tokens)?;
    let an_token =
        tokens.next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::AN)));
    let right_first_token = match tokens.next() {
        None => {
            return Err(ASTErrorType::from(ExpressionError::MissingOperands(
                operator.into_token(),
            )))
        }
        Some(token) => token,
    };
    let right = parse_expression(right_first_token, tokens)?;
    Ok(ASTExpression::BinaryOperation(BinaryOperation {
        operator,
        left: Box::new(left),
        right: Box::new(right),
        an_token,
    }))
}

fn parse_unary_expression(
    operator: UnaryOpt,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let first_token = match tokens.next() {
        None => {
            return Err(ASTErrorType::from(ExpressionError::MissingOperands(
                operator.into_token(),
            )))
        }
        Some(token) => token,
    };
    Ok(ASTExpression::UnaryOperation(UnaryOperation {
        operator,
        expression: Box::new(parse_expression(first_token, tokens)?),
    }))
}

fn parse_nary_expression(
    operator: NaryOpt,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let mut expressions: Vec<(ASTExpression, Option<Token>)> = Vec::new();
    while let Some(token) = tokens.next_if_token_type_ne(TokenType::Keyword(Keywords::MKAY)) {
        let operand = parse_expression(token, tokens)?;
        let an_token = tokens.next_if_token_type_eq(TokenType::Keyword(Keywords::AN));
        expressions.push((operand, an_token));
    }
    let mkay_token = tokens.next();
    Ok(ASTExpression::NaryOperation(NaryOperation {
        operator,
        expressions,
        mkay_token,
    }))
}
