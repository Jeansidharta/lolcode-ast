use crate::lexer::Token;
use crate::parser::expression::ASTExpression;
use crate::parser::statements::ASTErrorType;
use crate::parser::StatementIterator;

/// Represents a `FOUND YR` statement. This statement returns a value from a function.
/// ```LOLCode
/// HOW IZ I is_even YR num
///     MOD num AN 2, O RLY?
///         YA RLY, FOUND YR "Odd"
///         NO WAI, FOUND YR "Even"
///     OIC
/// IF U SAY SO
/// ```
#[derive(Debug, PartialEq, Clone)]
pub struct FoundYr {
    pub(crate) found_yr_token: Token,
    pub(crate) expression: ASTExpression,
}

impl FoundYr {
    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<FoundYr, ASTErrorType> {
        let next_token = match tokens.next() {
            Some(token) => token,
            None => return Err(ASTErrorType::MissingToken(first_token)),
        };
        let expression = ASTExpression::parse(next_token, tokens)?;

        Ok(FoundYr {
            found_yr_token: first_token,
            expression,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::{
        lexer::{Keywords, TokenType},
        parser::expression::{
            identifier::Identifier, ASTExpression, ASTExpressionValue, VariableAccess,
        },
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let keyword = Token::from(TokenType::Keyword(Keywords::GIMMEH));
        let ident = &keyword + TokenType::Identifier("Batata".to_string());

        assert_eq!(
            FoundYr::parse(
                keyword.clone(),
                &mut StatementIterator::new(vec![ident.clone()])
            ),
            Ok(FoundYr {
                found_yr_token: keyword.clone(),
                expression: ASTExpression::Value(ASTExpressionValue::VariableAccess(
                    VariableAccess {
                        identifier: Identifier {
                            name: ident.clone(),
                            srs: None
                        },
                        accesses: VecDeque::new(),
                    }
                ))
            })
        );
    }

    #[test]
    fn missing_token() {
        let keyword = Token::from(TokenType::Keyword(Keywords::GIMMEH));

        assert_eq!(
            FoundYr::parse(keyword.clone(), &mut StatementIterator::new(vec![])),
            Err(ASTErrorType::MissingToken(keyword.clone()))
        );
    }
}
