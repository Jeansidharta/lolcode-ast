mod binary;
mod n_ary;
mod unary;

use super::*;
use crate::lexer::*;
use pretty_assertions::assert_eq;

#[test]
fn complex_expressions() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Keyword(Keywords::AN), // Prefixed an
        TokenType::Keyword(Keywords::SMOOSH),
        TokenType::Value(TokenValue::String("A String".to_string())),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::String("Other String".to_string())),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::String("Some other string".to_string())),
        TokenType::Keyword(Keywords::AN), // A trailling an
        TokenType::Keyword(Keywords::MKAY),
        TokenType::Keyword(Keywords::NOT),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN), // Another trailling an
    ]);
    assert_eq!(
        parse_expression(operation, &mut operands.clone().into()),
        Ok(ASTExpression::SumOf(
            Rc::new(ASTExpression::Smoosh(VecDeque::from([
                ASTExpression::LiteralValue(operands.get(2).unwrap().clone()),
                ASTExpression::LiteralValue(operands.get(4).unwrap().clone()),
                ASTExpression::LiteralValue(operands.get(6).unwrap().clone()),
            ]))),
            Rc::new(ASTExpression::Not(Rc::new(ASTExpression::LiteralValue(
                operands.get(10).unwrap().clone()
            )))),
        ))
    );
}

#[test]
fn simple_bukkit_access() {
    let (first_token, tokens) = Token::chain_types(vec![
        TokenType::Identifier("Batata".to_string()),
        TokenType::BukkitSlotAccess,
        TokenType::Identifier("Tomate".to_string()),
        TokenType::BukkitSlotAccess,
        TokenType::Keyword(Keywords::SRS),
        TokenType::Identifier("Cebola".to_string()),
    ]);
    assert_eq!(
        parse_expression(first_token.clone(), &mut tokens.clone().into()),
        Ok(ASTExpression::VariableAccess(
            (
                (first_token.clone(), false),
                [(tokens[1].clone(), false), (tokens[4].clone(), true),]
            )
                .into()
        ))
    );
}
