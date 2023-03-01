use crate::lexer::Token;
use crate::parser::expression::variable_access::parse_variable_access;
use crate::parser::types::ASTErrorType;
use crate::parser::types::ASTNode;
use crate::parser::StatementIterator;

pub fn parse_gimmeh(
    first_token: Token,
    tokens: &mut StatementIterator,
) -> Result<ASTNode, ASTErrorType> {
    match tokens.next() {
        Some(token) => {
            parse_variable_access(token, tokens).and_then(|ident| Ok(ASTNode::Gimmeh(ident)))
        }
        None => Err(ASTErrorType::MissingToken(first_token)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lexer::{Keywords, TokenType, TokenValue::NOOB},
        parser::types::ASTErrorType,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple() {
        let keyword = Token::from(TokenType::Keyword(Keywords::GIMMEH));
        let ident = &keyword + TokenType::Identifier("Batata".to_string());

        assert_eq!(
            parse_gimmeh(keyword, &mut StatementIterator::new(vec![ident.clone()])),
            Ok(ASTNode::Gimmeh(((ident.clone(), false), []).into()))
        );
    }

    #[test]
    fn invalid_identifier() {
        let keyword = Token::from(TokenType::Keyword(Keywords::GIMMEH));
        let ident = &keyword + TokenType::Value(NOOB);

        assert_eq!(
            parse_gimmeh(keyword, &mut StatementIterator::new(vec![ident.clone()])),
            Err(ASTErrorType::UnexpectedToken(ident.clone()))
        );
    }

    #[test]
    fn missing_identifier() {
        let keyword = Token::from(TokenType::Keyword(Keywords::GIMMEH));

        assert_eq!(
            parse_gimmeh(keyword.clone(), &mut StatementIterator::new(vec![])),
            Err(ASTErrorType::MissingToken(keyword.clone()))
        );
    }
}
