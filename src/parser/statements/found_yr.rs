use crate::lexer::Token;
use crate::parser::expression::parse_expression;
use crate::parser::statements::ASTErrorType;
use crate::parser::statements::Node;
use crate::parser::StatementIterator;

pub(crate) fn parse_found_yr(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<Node, ASTErrorType> {
    let next_token = match tokens.next() {
        Some(token) => token,
        None => return Err(ASTErrorType::MissingToken(first_token)),
    };
    let expression = parse_expression(next_token, tokens)?;

    Ok(Node::FoundYr(expression))
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::{
        lexer::{Keywords, TokenType},
        parser::expression::{ASTExpression, ASTExpressionValue, Identifier, VariableAccess},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let keyword = Token::from(TokenType::Keyword(Keywords::GIMMEH));
        let ident = &keyword + TokenType::Identifier("Batata".to_string());

        assert_eq!(
            parse_found_yr(keyword, &mut StatementIterator::new(vec![ident.clone()])),
            Ok(Node::FoundYr(ASTExpression::Value(
                ASTExpressionValue::VariableAccess(VariableAccess {
                    identifier: Identifier {
                        name: ident.clone(),
                        srs: None
                    },
                    accesses: VecDeque::new(),
                })
            )))
        );
    }

    #[test]
    fn missing_token() {
        let keyword = Token::from(TokenType::Keyword(Keywords::GIMMEH));

        assert_eq!(
            parse_found_yr(keyword.clone(), &mut StatementIterator::new(vec![])),
            Err(ASTErrorType::MissingToken(keyword.clone()))
        );
    }
}
