use crate::{position::Position, range::Range};
pub use keywords::Keywords;
use std::fmt::Debug;

mod keywords;

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, PartialEq)]
pub enum NumberToken {
    Int(i32),
    Float(f32),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenValue {
    Number(NumberToken),
    String(String),
    Boolean(bool),
    NOOB,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Identifier(String),
    Keyword(Keywords),
    Symbol(String),
    Value(TokenValue),
    CommentSingleLine(String),
    CommentMultiLine(String),
    BukkitSlotAccess,
    Ellipsis,
    Comma,
    ExclamationMark,
    QuestionMark,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenError {
    UnclosedMultilineComment(Range),
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub range: Range,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}{:?}", self.range, self.token_type)
    }
}

fn parse_number(code: &str) -> Option<(&str, NumberToken)> {
    let mut has_dot_been_used = false;
    let position = code.chars().enumerate().position(|(index, char)| {
        if index as i32 == 0 && (char == '-' || char == '+') {
            return false;
        };
        if char >= '0' && char <= '9' {
            return false;
        };
        if !has_dot_been_used && char == '.' {
            has_dot_been_used = true;
            return false;
        };
        true
    })?;

    let number_str = &code[0..position];
    let number = if has_dot_been_used {
        NumberToken::Float(number_str.parse().ok()?)
    } else {
        NumberToken::Int(number_str.parse().ok()?)
    };
    return Some((number_str, number));
}

fn parse_keyword(code: &str) -> Option<(&str, Keywords)> {
    Keywords::iter()
        .find(|keyword| {
            let keyword_str = keyword.to_string_slice();
            code.starts_with(keyword_str)
        })
        .and_then(|keyword| {
            Some((
                &code[0..keyword.to_string_slice().len()],
                (*keyword).clone(),
            ))
        })
}

fn parse_identifier(code: &str) -> Option<&str> {
    let non_ident_position = code
        .chars()
        .into_iter()
        .take_while(|char| {
            *char == '_' || (*char >= 'a' && *char <= 'z') || (*char >= 'A' && *char <= 'Z')
        })
        .count();

    if non_ident_position == 0 {
        None
    } else {
        Some(&code[..non_ident_position])
    }
}

fn parse_string(code: &str) -> Option<(&str, &str)> {
    let mut chars = code.chars().enumerate().peekable();
    if let Some((_, char)) = chars.next() {
        if char != '"' {
            return None;
        };
    };

    while let Some((index, char)) = chars.next() {
        if char == ':' {
            if let Some((_, '"')) = chars.peek() {
                chars.next();
            }
        }

        if char == '"' {
            return Some((&code[..index + 1], &code[1..index]));
        };
    }

    Some((&code[..], &code[1..]))
}

fn parse_bukkit_slot_access(code: &str) -> Option<&str> {
    if code.starts_with("'Z") {
        Some(&code[..2])
    } else {
        None
    }
}

fn parse_whitespace(code: &str, position: &mut Position) -> usize {
    code.chars()
        .into_iter()
        .take_while(|char| {
            if *char == '\n' {
                position.line += 1;
                position.column = 1;
                position.bytes += char.len_utf8() as i32;
                position.chars += 1;
                true
            } else if *char == ' ' || *char == '\t' {
                position.column += 1;
                position.bytes += char.len_utf8() as i32;
                position.chars += 1;
                true
            } else {
                false
            }
        })
        .count()
}

fn parse_ellipsis(code: &str) -> Option<&str> {
    if code.starts_with("...") {
        Some(&code[..3])
    } else if code.starts_with('\u{2026}') {
        Some(&code[..1])
    } else {
        None
    }
}

fn parse_boolean(code: &str) -> Option<(&str, bool)> {
    let true_str = Keywords::WIN.to_string_slice();
    let false_str = Keywords::FAIL.to_string_slice();

    if code.starts_with(true_str) {
        Some((&code[..true_str.len()], true))
    } else if code.starts_with(false_str) {
        Some((&code[..false_str.len()], false))
    } else {
        None
    }
}

fn parse_noob(code: &str) -> Option<&str> {
    let noob_str = Keywords::NOOB.to_string_slice();

    if code.starts_with(noob_str) {
        Some(&code[..noob_str.len()])
    } else {
        None
    }
}

fn parse_single_line_comment(code: &str) -> Option<(&str, String)> {
    let btw_str = Keywords::BTW.to_string_slice();

    if code.starts_with(btw_str) {
        let end_len_bytes = code.find('\n').unwrap_or(code.len());
        let comment_contents = (&code[btw_str.len()..end_len_bytes]).to_string();
        Some((&code[..end_len_bytes], comment_contents))
    } else {
        None
    }
}

fn parse_multi_line_comment(code: &str) -> Result<Option<(&str, String)>, ()> {
    let obtw_str = Keywords::OBTW.to_string_slice();
    let tldr_str = Keywords::TLDR.to_string_slice();

    if code.starts_with(obtw_str) {
        let end_len_bytes = match code.find(tldr_str) {
            None => return Err(()),
            Some(bytes) => bytes,
        };

        let comment_contents = (&code[obtw_str.len()..end_len_bytes]).to_string();
        Ok(Some((
            &code[..end_len_bytes + tldr_str.len()],
            comment_contents,
        )))
    } else {
        Ok(None)
    }
}

fn make_token<'a>(
    code: &mut &'a str,
    current_position: &mut Position,
    raw_string: &'a str,
    token_type: TokenType,
) -> Token {
    let start_position = current_position.clone();
    *current_position += raw_string;
    *code = &code[raw_string.len()..];
    Token {
        range: (start_position, current_position.clone()).into(),
        token_type,
    }
}

pub fn tokenize(code: String) -> Result<Vec<Token>, TokenError> {
    let mut tokens = Vec::new();

    let mut code = &code[..];
    let mut current_position = Position {
        line: 1,
        column: 1,
        bytes: 0,
        chars: 0,
    };

    while code.len() > 0 {
        {
            let chars = parse_whitespace(code, &mut current_position);
            if chars > 0 {
                code = &code[chars..];
                continue;
            }
        }

        // Parse comma
        if code.starts_with(',') {
            let raw_comma = &code[..1];
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                raw_comma,
                TokenType::Comma,
            ));
            continue;
        }

        if code.starts_with('!') {
            let raw_exclamation_mark = &code[..1];
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                raw_exclamation_mark,
                TokenType::ExclamationMark,
            ));
            continue;
        }

        if code.starts_with(',') {
            let raw_question_mark = &code[..1];
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                raw_question_mark,
                TokenType::QuestionMark,
            ));
            continue;
        }

        if let Some((comment_str, comment)) = parse_single_line_comment(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                comment_str,
                TokenType::CommentSingleLine(comment),
            ));
            continue;
        }

        match parse_multi_line_comment(code) {
            Ok(Some((comment_str, comment))) => {
                tokens.push(make_token(
                    &mut code,
                    &mut current_position,
                    comment_str,
                    TokenType::CommentMultiLine(comment),
                ));
                continue;
            }
            Err(_) => {
                return Err(TokenError::UnclosedMultilineComment(
                    (current_position.clone(), current_position + code).into(),
                ))
            }
            Ok(None) => {}
        }

        if let Some(access_str) = parse_bukkit_slot_access(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                access_str,
                TokenType::BukkitSlotAccess,
            ));
            continue;
        }

        if let Some(ellipsis) = parse_ellipsis(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                ellipsis,
                TokenType::Ellipsis,
            ));
            continue;
        }

        if let Some((raw_boolean, value)) = parse_boolean(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                raw_boolean,
                TokenType::Value(TokenValue::Boolean(value)),
            ));
            continue;
        }

        if let Some(noob_str) = parse_noob(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                noob_str,
                TokenType::Value(TokenValue::NOOB),
            ));
            continue;
        }

        if let Some((keyword_str, keyword)) = parse_keyword(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                keyword_str,
                TokenType::Keyword(keyword),
            ));
            continue;
        }

        if let Some((number_str, number)) = parse_number(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                number_str,
                TokenType::Value(TokenValue::Number(number)),
            ));
            continue;
        }

        if let Some((raw_string, string)) = parse_string(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                raw_string,
                TokenType::Value(TokenValue::String(string.to_string())),
            ));
            continue;
        }

        if let Some(identifier) = parse_identifier(code) {
            tokens.push(make_token(
                &mut code,
                &mut current_position,
                identifier,
                TokenType::Identifier(identifier.to_string()),
            ));
            continue;
        }

        let raw_symbol = &code[..1];
        tokens.push(make_token(
            &mut code,
            &mut current_position,
            raw_symbol,
            TokenType::Symbol(raw_symbol.to_string()),
        ));
    }

    Ok(tokens)
}
