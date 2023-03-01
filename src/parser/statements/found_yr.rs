use crate::lexer::Token;
use crate::parser::expression::parse_expression;
use crate::parser::types::ASTErrorType;
use crate::parser::types::ASTNode;
use crate::parser::StatementIterator;

pub fn parse_found_yr(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTNode, ASTErrorType> {
    let next_token = match tokens.next() {
        Some(token) => token,
        None => return Err(ASTErrorType::MissingToken(first_token)),
    };
    let expression = parse_expression(next_token, tokens)?;

    Ok(ASTNode::FoundYr(expression))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lexer::{Keywords, TokenType},
        parser::types::{ASTErrorType, ASTExpression},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let keyword = Token::from(TokenType::Keyword(Keywords::GIMMEH));
        let ident = &keyword + TokenType::Identifier("Batata".to_string());

        assert_eq!(
            parse_found_yr(keyword, &mut StatementIterator::new(vec![ident.clone()])),
            Ok(ASTNode::FoundYr(ASTExpression::VariableAccess(
                ((ident.clone(), false), []).into()
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
