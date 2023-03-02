use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Keywords, Token, TokenType};
use crate::parser::blocks::{parse_block_switch, ASTBlock};
use crate::parser::expression::{parse_expression, ASTExpression};

use super::o_rly::ORlyError;

/// A `WTF?` statement, which is equivalent to a `switch` in other languages
#[derive(Debug, Clone, PartialEq)]
pub struct Wtf {
    /// The cases to test the given expression againsT
    pub omg: VecDeque<(ASTExpression, ASTBlock)>,
    /// If no `OMG` matches, use the `OMGWTF`
    pub omg_wtf: Option<ASTBlock>,
}

impl TryFrom<(Token, &mut StatementIterator)> for Wtf {
    type Error = ASTErrorType;
    fn try_from(
        (first_token, tokens): (Token, &mut StatementIterator),
    ) -> Result<Self, Self::Error> {
        match tokens.next().map(|t| t.token_type) {
            Some(TokenType::Symbol(symbol)) if symbol == "?" => {}
            _ => return Err(ORlyError::MissingQuestionMark(first_token).into()),
        };
        tokens.next_statement_should_be_empty()?;

        let mut omg = VecDeque::new();
        while let Some(omg_token) =
            tokens.next_if(|token| matches!(&token.token_type, TokenType::Keyword(Keywords::OMG)))
        {
            let first_token = match tokens.next() {
                None => return Err(ASTErrorType::MissingToken(omg_token)),
                Some(token) => token,
            };
            let expression = parse_expression(first_token, tokens)?;
            tokens.next_statement_should_be_empty()?;
            omg.push_back((expression, parse_block_switch(tokens)));
        }

        let omg_wtf = if tokens
            .next_if(|token| matches!(token.token_type, TokenType::Keyword(Keywords::OMGWTF)))
            .is_some()
        {
            tokens.next_statement_should_be_empty()?;
            Some(parse_block_switch(tokens))
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

        tokens.next_statement_should_be_empty()?;

        Ok(Wtf { omg, omg_wtf })
    }
}

impl Into<ASTNode> for Wtf {
    fn into(self) -> ASTNode {
        ASTNode::Wtf(self)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        lexer::{Keywords, TokenType, TokenValue},
        parser::statements::visible::Visible,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_switch() {
        let mut block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::WTF),
                TokenType::Symbol("?".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::OMG),
                TokenType::Value(TokenValue::Boolean(true)),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Identifier("WIN!!1!".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::OMG),
                TokenType::Value(TokenValue::Boolean(false)),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Identifier("FAIL :>(".to_string()),
            ],
            vec![
                TokenType::Keyword(Keywords::OMGWTF),
                TokenType::Comma,
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::NOOB),
            ],
            vec![TokenType::Keyword(Keywords::OIC)],
        ]);

        let first_token = block_tokens[0].pop_front().unwrap();

        assert_eq!(
            Wtf::try_from((first_token, &mut block_tokens.clone().into())),
            Ok(Wtf {
                omg: [
                    (
                        ASTExpression::LiteralValue(block_tokens[1][1].clone()),
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
                    (
                        ASTExpression::LiteralValue(block_tokens[3][1].clone()),
                        [Visible(
                            [ASTExpression::VariableAccess(
                                ((block_tokens[4][1].clone(), false), []).into(),
                            )]
                            .into(),
                            None
                        )
                        .into()]
                        .into()
                    )
                ]
                .into(),
                omg_wtf: Some(
                    [Visible(
                        [ASTExpression::LiteralValue(block_tokens[5][3].clone())].into(),
                        None
                    )
                    .into()]
                    .into()
                )
            })
        )
    }
}
