use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::statements::error_type::ASTErrorType;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

#[cfg(test)]
mod tests;

pub(crate) mod identifier;
pub(crate) mod variable_access;

pub use identifier::{ASTType, Identifier};
pub use variable_access::VariableAccess;

/// A LOLCODE expression.
#[derive(Debug, PartialEq, Clone)]
#[allow(missing_docs)]
pub enum ASTExpression {
    LiteralValue(Token),
    VariableAccess(VariableAccess),
    BothOf(Box<ASTExpression>, Box<ASTExpression>),
    EitherOf(Box<ASTExpression>, Box<ASTExpression>),
    WonOf(Box<ASTExpression>, Box<ASTExpression>),
    Not(Box<ASTExpression>),
    AllOf(VecDeque<ASTExpression>),
    AnyOf(VecDeque<ASTExpression>),
    SumOf(Box<ASTExpression>, Box<ASTExpression>),
    DiffOf(Box<ASTExpression>, Box<ASTExpression>),
    ProduktOf(Box<ASTExpression>, Box<ASTExpression>),
    QuoshuntOf(Box<ASTExpression>, Box<ASTExpression>),
    ModOf(Box<ASTExpression>, Box<ASTExpression>),
    BiggrOf(Box<ASTExpression>, Box<ASTExpression>),
    SmallrOf(Box<ASTExpression>, Box<ASTExpression>),
    BothSaem(Box<ASTExpression>, Box<ASTExpression>),
    Diffrint(Box<ASTExpression>, Box<ASTExpression>),
    Smoosh(VecDeque<ASTExpression>),
    Maek(Box<ASTExpression>, Token),
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
                .map(|v| ASTExpression::VariableAccess(v))
        }
        TokenType::Value(_) | TokenType::Keyword(Keywords::NOOB) => {
            Ok(ASTExpression::LiteralValue(first_token))
        }
        TokenType::Keyword(ref keyword) => {
            match keyword {
                // Boolean operations
                Keywords::BOTH_OF => parse_both_of(first_token, tokens),
                Keywords::EITHER_OF => parse_either_of(first_token, tokens),
                Keywords::WON_OF => parse_won_of(first_token, tokens),
                Keywords::NOT => parse_not(first_token, tokens),
                Keywords::ALL_OF => parse_all_of(first_token, tokens),
                Keywords::ANY_OF => parse_any_of(first_token, tokens),
                // Math operations
                Keywords::SUM_OF => parse_sum_of(first_token, tokens),
                Keywords::DIFF_OF => parse_diff_of(first_token, tokens),
                Keywords::PRODUKT_OF => parse_produkt_of(first_token, tokens),
                Keywords::QUOSHUNT_OF => parse_quoshunt_of(first_token, tokens),
                Keywords::MOD_OF => parse_mod_of(first_token, tokens),
                // Comparison operations
                Keywords::BIGGR_OF => parse_biggr_of(first_token, tokens),
                Keywords::SMALLR_OF => parse_smallr_of(first_token, tokens),
                Keywords::BOTH_SAEM => parse_both_saem(first_token, tokens),
                Keywords::DIFFRINT => parse_diffrint(first_token, tokens),
                // String concat operation
                Keywords::SMOOSH => parse_smoosh(first_token, tokens),
                // Type Cast operation
                Keywords::MAEK => parse_maek(first_token, tokens),

                _ => Err(ASTErrorType::UnexpectedToken(first_token)),
            }
        }
        _ => Err(ASTErrorType::UnexpectedToken(first_token)),
    }
}

fn parse_operand(
    operator_token: &Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    if let Some(first) = tokens.next() {
        match parse_expression(first, tokens) {
            Ok(e) => Ok(e),
            Err(ASTErrorType::UnexpectedToken(Token {
                token_type: TokenType::Keyword(Keywords::AN),
                ..
            })) => Err(ASTErrorType::Expression(ExpressionError::MissingOperands(
                operator_token.clone(),
            ))),
            Err(e) => Err(e),
        }
    } else {
        Err(ASTErrorType::Expression(ExpressionError::MissingOperands(
            operator_token.clone(),
        )))
    }
}

fn parse_optional_an(tokens: &mut StatementIterator) -> Option<Token> {
    tokens.next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::AN)))
}

fn parse_binary_operand(
    operator_token: &Token,
    tokens: &mut StatementIterator,
) -> Result<(Box<ASTExpression>, Box<ASTExpression>), ASTErrorType> {
    parse_optional_an(tokens);
    let first_operand = parse_operand(operator_token, tokens)?;
    parse_optional_an(tokens);
    let second_operand = parse_operand(operator_token, tokens)?;
    parse_optional_an(tokens);
    Ok((Box::new(first_operand), Box::new(second_operand)))
}

fn parse_infinite_operands(
    operator_token: &Token,
    tokens: &mut StatementIterator,
) -> Result<VecDeque<ASTExpression>, ASTErrorType> {
    let mut operands = VecDeque::new();
    parse_optional_an(tokens);
    while !matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::Keyword(Keywords::MKAY)) | None
    ) {
        let operand = parse_operand(operator_token, tokens)?;
        operands.push_back(operand);
        parse_optional_an(tokens);
    }

    if matches!(
        tokens.peek().map(|t| &t.token_type),
        Some(TokenType::Keyword(Keywords::MKAY))
    ) {
        tokens.next();
    }

    Ok(operands)
}

fn parse_both_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::BothOf(first_operand, second_operand))
}

fn parse_either_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::EitherOf(first_operand, second_operand))
}

fn parse_won_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::WonOf(first_operand, second_operand))
}

fn parse_not(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let operand = parse_operand(&first_token, tokens)?;
    Ok(ASTExpression::Not(Box::new(operand)))
}

fn parse_all_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let operands = parse_infinite_operands(&first_token, tokens)?;
    Ok(ASTExpression::AllOf(operands))
}

fn parse_any_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let operands = parse_infinite_operands(&first_token, tokens)?;
    Ok(ASTExpression::AnyOf(operands))
}

fn parse_sum_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::SumOf(first_operand, second_operand))
}

fn parse_diff_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::DiffOf(first_operand, second_operand))
}

fn parse_produkt_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::ProduktOf(first_operand, second_operand))
}

fn parse_quoshunt_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::QuoshuntOf(first_operand, second_operand))
}

fn parse_mod_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::ModOf(first_operand, second_operand))
}

fn parse_biggr_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::BiggrOf(first_operand, second_operand))
}

fn parse_smallr_of(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::SmallrOf(first_operand, second_operand))
}

fn parse_both_saem(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::BothSaem(first_operand, second_operand))
}

fn parse_diffrint(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let (first_operand, second_operand) = parse_binary_operand(&first_token, tokens)?;
    Ok(ASTExpression::Diffrint(first_operand, second_operand))
}

fn parse_smoosh(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let operands = parse_infinite_operands(&first_token, tokens)?;
    Ok(ASTExpression::Smoosh(operands))
}

fn parse_maek(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTExpression, ASTErrorType> {
    let first_token = match tokens.next() {
        None => return Err(ASTErrorType::MissingToken(first_token)),
        Some(token) => token,
    };
    let expression = parse_expression(first_token.clone(), tokens)?;
    match tokens.peek().map(|t| &t.token_type) {
        None => return Err(ASTErrorType::MissingToken(first_token)),
        Some(TokenType::Keyword(Keywords::A)) => {
            tokens.next();
        }
        _ => {}
    };
    let new_type = match tokens.next() {
        None => return Err(ASTErrorType::MissingToken(first_token)),
        Some(
            token @ Token {
                token_type:
                    TokenType::Keyword(
                        Keywords::NOOB
                        | Keywords::NUMBR
                        | Keywords::NUMBAR
                        | Keywords::YARN
                        | Keywords::TROOF,
                    ),
                ..
            },
        ) => token,
        Some(token) => return Err(ASTErrorType::UnexpectedToken(token)),
    };
    Ok(ASTExpression::Maek(Box::new(expression), new_type))
}
