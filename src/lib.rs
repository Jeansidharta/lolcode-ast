#![warn(missing_docs)]

//! An AST generator for LOLCODE focused on completeness
//!
//! This is a crate to generate a AST for LOLCODE source code.

use lexer::TokenError;

/// Transforms a LOLCODE code string into an array of tokens
pub mod lexer;
/// Transforms an array of tokens into an Abstract Syntax Tree (AST)
pub mod parser;

pub use lexer::tokenize;
pub use parser::parse;

/// Tokenize and then generate an AST for the given LOLCODE string.
///
/// ```rust
/// # use lolcode_ast::{
/// #     tokenize_and_parse,
/// #     parser::{
/// #         ASTBlock,
/// #         expression::{
/// #             VariableAccess,
/// #             Identifier
/// #         },
/// #         statements::ASTNode
/// #     },
/// #     lexer::{
/// #        Token,
/// #        Range,
/// #        Position,
/// #        TokenType
/// #     }
/// # };
/// # use std::collections::VecDeque;
///
/// let code = "GIMMEH VAR".to_string();
/// let ast = tokenize_and_parse(code);
///
/// assert_eq!(
///     ast,
///     Ok(
///         ASTBlock(VecDeque::from([
///             ASTNode::Gimmeh(
///                 VariableAccess {
///                     name: Identifier {
///                         name: Token {
///                             token_type: TokenType::Identifier("VAR".to_string()),
///                             range: Range (
///                                 Position {
///                                     line: 1,
///                                     column: 8,
///                                     bytes: 7,
///                                     chars: 7,
///                                 },
///                                 Position {
///                                     line: 1,
///                                     column: 11,
///                                     bytes: 10,
///                                     chars: 10,
///                                 }
///                             )
///                         },
///                         is_srs: false
///                     },
///                     accesses: VecDeque::new()
///                 }
///             )
///         ]))
///     )
/// );
/// ```
pub fn tokenize_and_parse(code: String) -> Result<parser::ASTBlock, TokenError> {
    let tokens = tokenize(code)?;
    Ok(parse(tokens))
}
