use super::*;
use pretty_assertions::assert_eq;

#[test]
fn unary_literal_value() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::NOT),
        TokenType::Value(TokenValue::NOOB),
    ]);

    assert_eq!(
        parse_operand(&operation, &mut operands.clone().into()),
        Ok(ASTExpression::LiteralValue(
            operands.get(0).unwrap().clone()
        ))
    );
}

#[test]
fn unary_idenficier() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::NOT),
        TokenType::Identifier("Batata".to_string()),
    ]);

    assert_eq!(
        parse_operand(&operation, &mut operands.clone().into()),
        Ok(ASTExpression::VariableAccess(
            ((operands[0].clone(), false), []).into()
        ))
    );
}
