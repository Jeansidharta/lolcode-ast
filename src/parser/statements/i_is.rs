use crate::parser::expression::variable_access::parse_variable_access;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;
use crate::parser::statements::VariableAccess;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::expression::{parse_expression, ASTExpression};

/// An `I IZ` statement, which is a function call
#[derive(Debug, Clone, PartialEq)]
pub struct IIz {
    /// The name of the function. Note that this is of type VariableAccess, which means it can not
    /// only be an identifier, but it can also be a bukkit access, and the function should be
    /// placed inside the bukkit.
    pub name: VariableAccess,
    /// A list of the arguments passed to the function.
    pub arguments: VecDeque<ASTExpression>,
}

/// Errors that can only happen in a `I IZ` statement
#[derive(Debug, PartialEq, Clone)]
pub enum IIzError {
    /// No name provided.
    ///
    /// Ex of error: `I IZ MKAY`
    /// correct usage: `I IZ function_name MKAY`
    MissingName(Token),
    /// There was an YR token, but no argument was found after it.
    ///
    /// Ex of error: `I IZ function_name YR MKAY`
    /// correct usage: `I IZ function_name YR argument MKAY`
    MissingArgument(Token),
    /// No MKAY token was found at the end of the call
    ///
    /// Ex of error: `I IZ function_name YR argument`
    /// correct usage: `I IZ function_name YR argument MKAY`
    MissingMkay(Token),
}

impl Into<ASTErrorType> for IIzError {
    fn into(self) -> ASTErrorType {
        ASTErrorType::IIz(self)
    }
}

impl TryFrom<(Token, &mut StatementIterator)> for IIz {
    type Error = ASTErrorType;

    fn try_from(
        (first_token, tokens): (Token, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
        let name = match tokens.next() {
            None => return Err(ASTErrorType::IIz(IIzError::MissingName(first_token))),
            Some(token) => parse_variable_access(token, tokens)?,
        };

        let mut arguments = VecDeque::new();
        loop {
            let yr_token = match tokens
                .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::YR)))
            {
                Some(token) => token,
                None => break,
            };
            let first_expression_token = match tokens.next() {
                Some(token) => token,
                None => return Err(ASTErrorType::IIz(IIzError::MissingArgument(yr_token))),
            };
            arguments.push_back(parse_expression(first_expression_token, tokens)?);
            if tokens
                .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::AN)))
                .is_none()
            {
                break;
            }
        }
        match tokens.next() {
            None => return Err(ASTErrorType::IIz(IIzError::MissingMkay(first_token))),
            Some(Token {
                token_type: TokenType::Keyword(Keywords::MKAY),
                ..
            }) => {}
            Some(token) => return Err(ASTErrorType::IIz(IIzError::MissingMkay(token))),
        }

        Ok(IIz { name, arguments })
    }
}

impl Into<ASTNode> for IIz {
    fn into(self) -> ASTNode {
        ASTNode::IIz(self)
    }
}

#[cfg(test)]
mod tests {

    use std::rc::Rc;

    use super::*;
    use crate::lexer::{Keywords, TokenType};
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_no_arguments() {
        let mut block_tokens = Token::make_block(vec![vec![
            TokenType::Keyword(Keywords::I_IZ),
            TokenType::Identifier("Some_Func".to_string()),
            TokenType::Keyword(Keywords::MKAY),
        ]]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            IIz::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(IIz {
                name: ((block_tokens[0][0].clone(), false), []).into(),
                arguments: [].into()
            })
        )
    }

    #[test]
    fn simple_some_arguments() {
        let mut block_tokens = Token::make_block(vec![vec![
            TokenType::Keyword(Keywords::I_IZ),
            TokenType::Identifier("Some_Func".to_string()),
            TokenType::Keyword(Keywords::YR),
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::AN),
            TokenType::Keyword(Keywords::YR),
            TokenType::Identifier("Tomate".to_string()),
            TokenType::Keyword(Keywords::AN),
            TokenType::Keyword(Keywords::YR),
            TokenType::Keyword(Keywords::SUM_OF),
            TokenType::Identifier("Cebola".to_string()),
            TokenType::Keyword(Keywords::AN),
            TokenType::Keyword(Keywords::NOOB),
            TokenType::Keyword(Keywords::MKAY),
        ]]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            IIz::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(IIz {
                name: ((block_tokens[0][0].clone(), false), []).into(),
                arguments: [
                    ASTExpression::VariableAccess(((block_tokens[0][2].clone(), false), []).into()),
                    ASTExpression::VariableAccess(((block_tokens[0][5].clone(), false), []).into()),
                    ASTExpression::SumOf(
                        Rc::new(ASTExpression::VariableAccess(
                            ((block_tokens[0][9].clone(), false), []).into()
                        )),
                        Rc::new(ASTExpression::LiteralValue(block_tokens[0][11].clone()))
                    )
                ]
                .into()
            })
        )
    }
}
