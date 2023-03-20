use super::*;
use pretty_assertions::assert_eq;

#[test]
fn binary_normal_usecase() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN),
        TokenType::Identifier("Batata".to_string()),
    ]);

    assert_eq!(
        parse_binary_operand(&operation, &mut operands.clone().into()),
        Ok((
            Box::new(ASTExpression::LiteralValue(operands[0].clone())),
            Box::new(ASTExpression::VariableAccess(VariableAccess {
                identifier: (operands[2].clone(), false).into(),
                accesses: [].into()
            })),
        ))
    );
}

#[test]
fn binary_no_an() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Identifier("Batata".to_string()),
    ]);

    assert_eq!(
        parse_binary_operand(&operation, &mut operands.clone().into()),
        Ok((
            Box::new(ASTExpression::LiteralValue(operands[0].clone())),
            Box::new(ASTExpression::VariableAccess(VariableAccess {
                identifier: (operands[1].clone(), false).into(),
                accesses: [].into()
            })),
        ))
    );
}

#[test]
fn binary_trailling_an() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN),
        TokenType::Identifier("Batata".to_string()),
        TokenType::Keyword(Keywords::AN),
    ]);

    assert_eq!(
        parse_binary_operand(&operation, &mut operands.clone().into()),
        Ok((
            Box::new(ASTExpression::LiteralValue(operands[0].clone())),
            Box::new(ASTExpression::VariableAccess(VariableAccess {
                identifier: (operands[2].clone(), false).into(),
                accesses: [].into()
            })),
        ))
    );
}

#[test]
fn binary_prefix_an() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN),
        TokenType::Identifier("Batata".to_string()),
        TokenType::Keyword(Keywords::AN),
    ]);

    assert_eq!(
        parse_binary_operand(&operation, &mut operands.clone().into()),
        Ok((
            Box::new(ASTExpression::LiteralValue(operands[1].clone())),
            Box::new(ASTExpression::VariableAccess(
                ((operands[3].clone(), false), []).into()
            )),
        ))
    );
}

#[test]
fn binary_missing_first_operand() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        // TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN),
        TokenType::Identifier("Batata".to_string()),
    ]);
    assert_eq!(
        parse_binary_operand(&operation, &mut operands.clone().into()),
        Err(ASTErrorType::Expression(ExpressionError::MissingOperands(
            operation
        )))
    );
}

#[test]
fn binary_missing_second_operand() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN),
        // TokenType::Identifier("Batata".to_string()),
    ]);
    assert_eq!(
        parse_binary_operand(&operation, &mut operands.clone().into()),
        Err(ASTErrorType::Expression(ExpressionError::MissingOperands(
            operation
        )))
    );
}
