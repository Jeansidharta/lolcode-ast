use crate::parser::expression::variable_access::parse_identifier;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::Node;
use crate::parser::Identifier;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::{parse_block_function, ASTBlock};

/// A `HOW IZ I` statement, which is used to define a function.
#[derive(Clone, Debug, PartialEq)]
pub struct HowIzI {
    /// The function name
    pub name: Identifier,
    /// A list of the function's arguments
    pub arguments: VecDeque<Identifier>,
    /// The function's body
    pub body: ASTBlock,
}

/// Errors that can only happen when parsing a `HOW IZ I` statement
#[derive(Debug, PartialEq, Clone)]
pub enum HowIsIError {
    /// A `HOW IZ I` token was found, but no Identifier after
    MissingNameIdentifier(Token),
}

impl Into<ASTErrorType> for HowIsIError {
    fn into(self) -> ASTErrorType {
        ASTErrorType::HowIsIError(self)
    }
}

impl TryFrom<(Token, &mut StatementIterator)> for HowIzI {
    type Error = ASTErrorType;

    fn try_from(
        (first_token, tokens): (Token, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
        let name = match tokens.next() {
            None => return Err(HowIsIError::MissingNameIdentifier(first_token).into()),
            Some(token) => parse_identifier(token, tokens)?,
        };

        let mut arguments = VecDeque::new();
        loop {
            let yr_token = match tokens
                .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::YR)))
            {
                Some(token) => token,
                None => break,
            };
            let param = match tokens.next() {
                None => return Err(ASTErrorType::MissingToken(yr_token)),
                Some(token) => parse_identifier(token, tokens)?,
            };
            arguments.push_back(param);
            if !matches!(
                tokens.peek().map(|t| &t.token_type),
                Some(TokenType::Keyword(Keywords::AN))
            ) {
                break;
            }
            tokens.next();
        }
        tokens.next_statement_should_be_empty()?;

        let body = parse_block_function(tokens);

        match tokens.next() {
            None => return Err(ASTErrorType::MissingBlockClosingToken(first_token)),
            Some(Token {
                token_type: TokenType::Keyword(Keywords::IF_U_SAY_SO),
                ..
            }) => {}
            Some(token) => return Err(ASTErrorType::UnexpectedToken(token)),
        }

        tokens.next_statement_should_be_empty()?;

        Ok(HowIzI {
            body,
            name,
            arguments,
        })
    }
}

impl Into<Node> for HowIzI {
    fn into(self) -> Node {
        Node::HowIzI(self)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        lexer::{Keywords, TokenType, TokenValue},
        parser::{expression::ASTExpression, statements::visible::Visible},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_no_arguments() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::HOW_IZ_I),
                TokenType::Identifier("Some_Func".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::String("Hai!".to_string())),
            ],
            vec![TokenType::Keyword(Keywords::IF_U_SAY_SO)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            HowIzI::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(HowIzI {
                name: (block_tokens[0][0].clone(), false).into(),
                arguments: [].into(),
                body: [Visible(
                    [ASTExpression::LiteralValue(block_tokens[1][1].clone())].into(),
                    None
                )
                .into()]
                .into()
            })
        )
    }

    #[test]
    fn simple_some_arguments() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::HOW_IZ_I),
                TokenType::Identifier("Some_Func".to_string()),
                TokenType::Keyword(Keywords::YR),
                TokenType::Identifier("Arg1".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Keyword(Keywords::YR),
                TokenType::Identifier("Arg2".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Keyword(Keywords::YR),
                TokenType::Identifier("Arg3".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::String("Hai!".to_string())),
            ],
            vec![TokenType::Keyword(Keywords::IF_U_SAY_SO)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            HowIzI::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(HowIzI {
                name: (block_tokens[0][0].clone(), false).into(),
                arguments: [
                    (block_tokens[0][2].clone(), false).into(),
                    (block_tokens[0][5].clone(), false).into(),
                    (block_tokens[0][8].clone(), false).into(),
                ]
                .into(),
                body: [Visible(
                    [ASTExpression::LiteralValue(block_tokens[1][1].clone())].into(),
                    None
                )
                .into()]
                .into()
            })
        )
    }
}
