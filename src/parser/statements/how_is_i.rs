use crate::parser::expression::identifier::Identifier;
use crate::parser::statements::ASTErrorType;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::{parse_block_function, ASTBlock};

/// A `HOW IZ I` statement, which is used to define a function.
#[derive(Clone, Debug, PartialEq)]
pub struct HowIzI {
    pub(crate) how_iz_i_token: Token,
    /// The function name
    pub(crate) name: Identifier,
    /// A list of the function's arguments
    pub(crate) arguments: VecDeque<Identifier>,
    /// The function's body
    pub(crate) body: ASTBlock,
}

/// Errors that can only happen when parsing a `HOW IZ I` statement
#[derive(Debug, PartialEq, Clone)]
pub enum HowIsIError {
    /// A `HOW IZ I` token was found, but no Identifier after
    MissingNameIdentifier(Token),
}

impl HowIzI {
    pub(crate) fn parse(
        first_token: Token,
        tokens: &mut StatementIterator,
    ) -> Result<HowIzI, ASTErrorType> {
        let name = match tokens.next() {
            None => return Err(HowIsIError::MissingNameIdentifier(first_token).into()),
            Some(token) => Identifier::parse(token, tokens)?,
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
                Some(token) => Identifier::parse(token, tokens)?,
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
            how_iz_i_token: first_token,
            body,
            name,
            arguments,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        lexer::{Keywords, TokenType, TokenValue},
        parser::{
            expression::{identifier::Identifier, ASTExpression, ASTExpressionValue},
            statements::{visible::Visible, Node},
        },
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
            HowIzI::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Ok(HowIzI {
                how_iz_i_token: first_token.clone(),
                name: Identifier {
                    name: block_tokens[0][0].clone(),
                    srs: None
                },
                arguments: VecDeque::new(),
                body: ASTBlock(VecDeque::from([Node::Visible(Visible {
                    visible_token: block_tokens[1][0].clone(),
                    expressions: VecDeque::from([ASTExpression::Value(
                        ASTExpressionValue::LiteralValue(block_tokens[1][1].clone())
                    )]),
                    exclamation_mark: None
                })]))
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
            HowIzI::parse(first_token.clone(), &mut block_tokens.clone().into()),
            Ok(HowIzI {
                how_iz_i_token: first_token.clone(),
                name: Identifier {
                    name: block_tokens[0][0].clone(),
                    srs: None
                },
                arguments: VecDeque::from([
                    Identifier {
                        name: block_tokens[0][2].clone(),
                        srs: None
                    },
                    Identifier {
                        name: block_tokens[0][5].clone(),
                        srs: None
                    },
                    Identifier {
                        name: block_tokens[0][8].clone(),
                        srs: None
                    },
                ]),
                body: ASTBlock(VecDeque::from([Node::Visible(Visible {
                    visible_token: block_tokens[1][0].clone(),
                    expressions: VecDeque::from([ASTExpression::Value(
                        ASTExpressionValue::LiteralValue(block_tokens[1][1].clone())
                    )]),
                    exclamation_mark: None
                })]))
            })
        )
    }
}
