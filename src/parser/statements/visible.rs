use crate::parser::statements::ASTErrorType;
use crate::parser::statements::ASTNode;
use crate::parser::StatementIterator;
use std::collections::VecDeque;

use crate::lexer::{Token, TokenType};
use crate::parser::expression::{parse_expression, ASTExpression};

#[derive(Debug, Clone, PartialEq)]
pub struct Visible(pub VecDeque<ASTExpression>, pub Option<Token>);

impl TryFrom<&mut StatementIterator> for Visible {
    type Error = ASTErrorType;
    fn try_from(tokens: &mut StatementIterator) -> Result<Self, Self::Error> {
        let mut expressions: VecDeque<ASTExpression> = VecDeque::new();
        while let Some(token) = tokens.next_if_token_type_ne(TokenType::ExclamationMark) {
            expressions.push_back(parse_expression(token, tokens)?);
        }

        let exclamation_mark = tokens.next_if_token_type_eq(TokenType::ExclamationMark);

        Ok(Visible(expressions, exclamation_mark))
    }
}

impl Into<ASTNode> for Visible {
    fn into(self) -> ASTNode {
        ASTNode::Visible(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Keywords, NumberToken::Int, Token, TokenValue};
    use pretty_assertions::assert_eq;
    use std::rc::Rc;

    #[test]
    fn success_hello_world() {
        let (_keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::VISIBLE),
            TokenType::Value(TokenValue::String("Hello, World!".to_string())),
        ]);

        assert_eq!(
            Visible::try_from(&mut tokens.clone().into()),
            Ok(Visible(
                [ASTExpression::LiteralValue(tokens[0].clone())].into(),
                None
            ))
        );
    }

    #[test]
    fn success_hello_world_exclamation_mark() {
        let (_keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::VISIBLE),
            TokenType::Value(TokenValue::String("Hello, World!".to_string())),
            TokenType::ExclamationMark,
        ]);

        assert_eq!(
            Visible::try_from(&mut tokens.clone().into()),
            Ok(Visible(
                [ASTExpression::LiteralValue(tokens[0].clone())].into(),
                Some(tokens[1].clone())
            ))
        );
    }

    #[test]
    fn success_empty_expression() {
        let (_keyword, tokens) = Token::chain_types(vec![TokenType::Keyword(Keywords::VISIBLE)]);

        assert_eq!(
            Visible::try_from(&mut tokens.clone().into()),
            Ok(Visible([].into(), None))
        );
    }

    #[test]
    fn success_empty_expression_with_exclamation_mark() {
        let (_keyword, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::VISIBLE),
            TokenType::ExclamationMark,
        ]);

        assert_eq!(
            Visible::try_from(&mut tokens.clone().into()),
            Ok(Visible([].into(), Some(tokens[0].clone())))
        );
    }

    #[test]
    fn complex_expressions() {
        let (_first_token, tokens) = Token::chain_types(vec![
            TokenType::Keyword(Keywords::VISIBLE),
            TokenType::Keyword(Keywords::SMOOSH),
            TokenType::Value(TokenValue::String("Hello, World!".to_string())),
            TokenType::Keyword(Keywords::AN),
            TokenType::Keyword(Keywords::SUM_OF),
            TokenType::Value(TokenValue::Number(Int(10))),
            TokenType::Value(TokenValue::NOOB),
            TokenType::Identifier("Batata".to_string()),
            TokenType::Keyword(Keywords::MKAY),
            TokenType::Keyword(Keywords::EITHER_OF),
            TokenType::Identifier("Tomate".to_string()),
            TokenType::Keyword(Keywords::AN),
            TokenType::Value(TokenValue::NOOB),
            TokenType::ExclamationMark,
        ]);

        assert_eq!(
            Visible::try_from(&mut StatementIterator::new(tokens.clone().into())),
            Ok(Visible(
                [
                    ASTExpression::Smoosh(
                        [
                            ASTExpression::LiteralValue(tokens[1].clone()),
                            ASTExpression::SumOf(
                                Rc::new(ASTExpression::LiteralValue(tokens[4].clone())),
                                Rc::new(ASTExpression::LiteralValue(tokens[5].clone()))
                            ),
                            ASTExpression::VariableAccess(((tokens[6].clone(), false), []).into())
                        ]
                        .into()
                    ),
                    ASTExpression::EitherOf(
                        Rc::new(ASTExpression::VariableAccess(
                            ((tokens[9].clone(), false), []).into()
                        )),
                        Rc::new(ASTExpression::LiteralValue(tokens[11].clone()))
                    )
                ]
                .into(),
                Some(tokens[12].clone())
            ))
        );
    }
}
