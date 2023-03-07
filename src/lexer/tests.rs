use std::collections::VecDeque;

use super::*;

#[cfg(test)]
// This is only used on tests
impl TokenType {
    pub fn to_string(&self) -> String {
        match self {
            TokenType::BukkitSlotAccess => "'Z".into(),
            TokenType::Comma => ",".into(),
            TokenType::ExclamationMark => "!".into(),
            TokenType::QuestionMark => "?".into(),
            TokenType::Ellipsis => "...".into(),
            TokenType::Identifier(ident) => ident.clone(),
            TokenType::Keyword(keyword) => keyword.into_str().to_string(),
            TokenType::Symbol(symbol) => symbol.to_string(),
            TokenType::CommentSingleLine(comment) => format!("BTW{}", comment),
            TokenType::CommentMultiLine(comment) => format!("OBTW{}TLDR", comment),
            TokenType::Value(value) => match value {
                TokenValue::NOOB => "NOOB".to_string(),
                TokenValue::Number(NumberToken::Int(num)) => num.to_string(),
                TokenValue::Number(NumberToken::Float(num)) => num.to_string(),
                TokenValue::String(str) => format!("\"{}\"", str),
                TokenValue::Boolean(b) => b.to_string(),
            },
        }
    }
}

#[cfg(test)]
// This is only used on tests
impl std::ops::Add<TokenType> for &Token {
    type Output = Token;
    fn add(self, token_type: TokenType) -> Token {
        let type_str = token_type.to_string();
        let range = self.range.after() + " " + &type_str;
        Token { range, token_type }
    }
}

#[cfg(test)]
// This is only used on tests
impl Token {
    pub fn make_line(types: Vec<TokenType>, initial_line: i32) -> VecDeque<Token> {
        types.into_iter().fold(VecDeque::new(), |mut acc, item| {
            acc.push_back(match acc.back() {
                Some(back) => back + item,
                None => {
                    let mut token = Token::from(item);
                    token.range.set_line(initial_line);
                    token
                }
            });
            acc
        })
    }

    pub fn chain_types(types: Vec<TokenType>) -> (Token, VecDeque<Token>) {
        let mut line = Token::make_line(types, 1);
        (line.pop_front().unwrap(), line)
    }

    pub fn make_block(types: Vec<Vec<TokenType>>) -> VecDeque<VecDeque<Token>> {
        types
            .into_iter()
            .enumerate()
            .fold(VecDeque::new(), |mut acc, (index, line)| {
                let line_tokens = Token::make_line(line, index as i32);
                acc.push_back(line_tokens);
                acc
            })
    }
}

#[cfg(test)]
impl From<TokenType> for Token {
    fn from(token_type: TokenType) -> Self {
        let token_len = token_type.to_string().len() as i32;
        Token {
            range: (
                Position::default(),
                Position {
                    column: 1 + token_len,
                    bytes: token_len,
                    chars: token_len,
                    line: 1,
                },
            )
                .into(),
            token_type,
        }
    }
}
