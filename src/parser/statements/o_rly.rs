use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::{parse_block_if, ASTBlock};

#[derive(Clone, PartialEq, Debug)]
pub struct ORly {
    pub if_true: Option<ASTBlock>,
    pub if_false: Option<ASTBlock>,
    pub mebbes: VecDeque<ASTBlock>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ORlyError {
    MissingQuestionMark(Token),
}

impl Into<ASTErrorType> for ORlyError {
    fn into(self) -> ASTErrorType {
        ASTErrorType::ORlyError(self)
    }
}

impl TryFrom<(Token, &mut StatementIterator)> for ORly {
    type Error = ASTErrorType;

    fn try_from(
        (first_token, tokens): (Token, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
        match tokens.next().map(|t| t.token_type) {
            Some(TokenType::Symbol(symbol)) if symbol == "?" => {}
            _ => return Err(ORlyError::MissingQuestionMark(first_token).into()),
        };
        tokens.next_statement_should_be_empty()?;

        let if_true = if tokens
            .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::YA_RLY)))
            .is_some()
        {
            tokens.next_statement_should_be_empty()?;
            Some(parse_block_if(tokens))
        } else {
            None
        };

        let mut mebbes = VecDeque::new();
        while tokens
            .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::MEBBE)))
            .is_some()
        {
            tokens.next_statement_should_be_empty()?;
            mebbes.push_back(parse_block_if(tokens));
        }

        let if_false = if tokens
            .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::NO_WAI)))
            .is_some()
        {
            tokens.next_statement_should_be_empty()?;
            Some(parse_block_if(tokens))
        } else {
            None
        };

        match tokens.next() {
            None => return Err(ASTErrorType::MissingBlockClosingToken(first_token)),
            Some(Token {
                token_type: TokenType::Keyword(Keywords::OIC),
                ..
            }) => {}
            Some(token) => return Err(ASTErrorType::UnexpectedToken(token)),
        }

        Ok(ORly {
            if_true,
            if_false,
            mebbes,
        })
    }
}

impl Into<ASTNode> for ORly {
    fn into(self) -> ASTNode {
        ASTNode::ORly(self)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        lexer::{Keywords, NumberToken, TokenType, TokenValue},
        parser::{expression::ASTExpression, statements::visible::Visible},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn if_mebbe_else() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::O_RLY),
                TokenType::Symbol("?".to_string()),
            ],
            vec![TokenType::Keyword(Keywords::YA_RLY)],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Identifier("Batata".to_string()),
            ],
            vec![TokenType::Keyword(Keywords::MEBBE)],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::NOOB),
            ],
            vec![
                TokenType::Keyword(Keywords::NO_WAI),
                TokenType::Comma,
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::Number(NumberToken::Int(10))),
            ],
            vec![TokenType::Keyword(Keywords::OIC)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ORly::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(ORly {
                if_true: Some(
                    [Visible(
                        [ASTExpression::VariableAccess(
                            ((block_tokens[2][1].clone(), false), []).into(),
                        )]
                        .into(),
                        None
                    )
                    .into()]
                    .into()
                ),
                mebbes: [[Visible(
                    [ASTExpression::LiteralValue(block_tokens[4][1].clone())].into(),
                    None
                )
                .into()]
                .into()]
                .into(),
                if_false: Some(
                    [Visible(
                        [ASTExpression::LiteralValue(block_tokens[5][3].clone())].into(),
                        None
                    )
                    .into()]
                    .into()
                ),
            })
        )
    }

    #[test]
    fn if_else() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::O_RLY),
                TokenType::Symbol("?".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::NO_WAI),
                TokenType::Comma,
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::Number(NumberToken::Int(10))),
            ],
            vec![TokenType::Keyword(Keywords::OIC)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ORly::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(ORly {
                if_true: None,
                mebbes: [].into(),
                if_false: Some(
                    [Visible(
                        [ASTExpression::LiteralValue(block_tokens[1][3].clone())].into(),
                        None
                    )
                    .into()]
                    .into()
                ),
            })
        )
    }

    #[test]
    fn missing_question_mark() {
        let mut block_tokens = Token::make_block(vec![
            vec![TokenType::Keyword(Keywords::O_RLY)],
            vec![
                TokenType::Keyword(Keywords::NO_WAI),
                TokenType::Comma,
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::Number(NumberToken::Int(10))),
            ],
            vec![TokenType::Keyword(Keywords::OIC)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            ORly::try_from((first_token.clone(), &mut block_tokens.clone().into())),
            Err(ORlyError::MissingQuestionMark(first_token.clone()).into())
        )
    }
}
