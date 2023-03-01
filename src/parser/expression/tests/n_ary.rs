use super::*;
use pretty_assertions::assert_eq;

#[test]
fn normal_usage() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN),
        TokenType::Identifier("Batata".to_string()),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::String("A String".to_string())),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::Number(NumberToken::Int(128))),
    ]);
    assert_eq!(
        parse_infinite_operands(&operation, &mut operands.clone().into()),
        Ok(VecDeque::from([
            ASTExpression::LiteralValue(operands[0].clone()),
            ASTExpression::VariableAccess(((operands[2].clone(), false), []).into()),
            ASTExpression::LiteralValue(operands[4].clone()),
            ASTExpression::LiteralValue(operands[6].clone()),
        ]))
    );
}

#[test]
fn trailling_an() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN),
        TokenType::Identifier("Batata".to_string()),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::String("A String".to_string())),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::Number(NumberToken::Int(128))),
        TokenType::Keyword(Keywords::AN),
    ]);
    assert_eq!(
        parse_infinite_operands(&operation, &mut operands.clone().into()),
        Ok(VecDeque::from([
            ASTExpression::LiteralValue(operands[0].clone()),
            ASTExpression::VariableAccess(((operands[2].clone(), false), []).into()),
            ASTExpression::LiteralValue(operands[4].clone()),
            ASTExpression::LiteralValue(operands[6].clone()),
        ]))
    );
}

#[test]
fn prepended_an() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),        // operation
        TokenType::Keyword(Keywords::AN),            // 0
        TokenType::Value(TokenValue::NOOB),          // 1
        TokenType::Keyword(Keywords::AN),            // 2
        TokenType::Identifier("Batata".to_string()), // 3
        TokenType::Keyword(Keywords::AN),            // 4
        TokenType::Value(TokenValue::String("A String".to_string())), // 5
        TokenType::Keyword(Keywords::AN),            // 6
        TokenType::Value(TokenValue::Number(NumberToken::Int(128))), // 7
        TokenType::Keyword(Keywords::AN),
    ]);
    assert_eq!(
        parse_infinite_operands(&operation, &mut operands.clone().into()),
        Ok(VecDeque::from([
            ASTExpression::LiteralValue(operands[1].clone()),
            ASTExpression::VariableAccess(((operands[3].clone(), false), []).into(),),
            ASTExpression::LiteralValue(operands[5].clone()),
            ASTExpression::LiteralValue(operands[7].clone()),
        ]))
    );
}

#[test]
fn no_and() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Identifier("Batata".to_string()),
        TokenType::Value(TokenValue::String("A String".to_string())),
        TokenType::Value(TokenValue::Number(NumberToken::Int(128))),
    ]);
    assert_eq!(
        parse_infinite_operands(&operation, &mut operands.clone().into()),
        Ok(VecDeque::from([
            ASTExpression::LiteralValue(operands[0].clone()),
            ASTExpression::VariableAccess(((operands[1].clone(), false), []).into(),),
            ASTExpression::LiteralValue(operands[2].clone()),
            ASTExpression::LiteralValue(operands[3].clone()),
        ]))
    );
}

#[test]
fn mkay_at_and() {
    let (operation, operands) = Token::chain_types(vec![
        TokenType::Keyword(Keywords::SUM_OF),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::NOOB),
        TokenType::Keyword(Keywords::AN),
        TokenType::Identifier("Batata".to_string()),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::String("A String".to_string())),
        TokenType::Keyword(Keywords::MKAY),
        TokenType::Keyword(Keywords::AN),
        TokenType::Value(TokenValue::Number(NumberToken::Int(128))),
        TokenType::Keyword(Keywords::AN),
    ]);
    assert_eq!(
        parse_infinite_operands(&operation, &mut operands.clone().into()),
        Ok(VecDeque::from([
            ASTExpression::LiteralValue(operands[1].clone()),
            ASTExpression::VariableAccess(((operands[3].clone(), false), []).into(),),
            ASTExpression::LiteralValue(operands[5].clone()),
        ]))
    );
}
