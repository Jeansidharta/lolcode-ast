use crate::parser::statements::ASTNode;
use std::collections::VecDeque;
use std::ops::{Deref, DerefMut};

use crate::parser::StatementIterator;

use crate::lexer::{Keywords, Token, TokenType};

use super::parse_statement;

/// A block of code, which is an array of statements
#[derive(Debug, PartialEq, Clone)]
pub struct ASTBlock(pub VecDeque<ASTNode>);

impl Default for ASTBlock {
    fn default() -> Self {
        Self(VecDeque::new())
    }
}

impl<const T: usize> From<[ASTNode; T]> for ASTBlock {
    fn from(value: [ASTNode; T]) -> Self {
        Self(VecDeque::from(value))
    }
}

impl Deref for ASTBlock {
    type Target = VecDeque<ASTNode>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ASTBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

fn parse_block(
    statement_iterator: &mut StatementIterator,
    should_stop_at_token: &dyn Fn(&Token) -> bool,
) -> ASTBlock {
    let mut block = ASTBlock::default();
    loop {
        match statement_iterator.next() {
            Some(token) => block.push_back(
                parse_statement(token, statement_iterator).unwrap_or_else(|err| err.into()),
            ),
            None => {}
        };

        statement_iterator
            .next_statement_should_be_empty()
            .unwrap_or_else(|err| block.push_back(err.into()));

        match statement_iterator.peek() {
            Some(token) if should_stop_at_token(token) => break,
            None => break,
            Some(_) => {}
        }
    }
    block
}

pub fn parse_block_root(statement_iterator: &mut StatementIterator) -> ASTBlock {
    parse_block(statement_iterator, &|_| false)
}

pub fn parse_block_function(statement_iterator: &mut StatementIterator) -> ASTBlock {
    parse_block(statement_iterator, &|token| match token.token_type {
        TokenType::Keyword(Keywords::IF_U_SAY_SO) => true,
        _ => false,
    })
}

pub fn parse_block_loop(statement_iterator: &mut StatementIterator) -> ASTBlock {
    parse_block(statement_iterator, &|token| match token.token_type {
        TokenType::Keyword(Keywords::IM_OUTTA_YR) => true,
        _ => false,
    })
}

pub fn parse_block_if(statement_iterator: &mut StatementIterator) -> ASTBlock {
    parse_block(statement_iterator, &|token| match token.token_type {
        TokenType::Keyword(
            Keywords::MEBBE | Keywords::NO_WAI | Keywords::YA_RLY | Keywords::OIC,
        ) => true,
        _ => false,
    })
}

pub fn parse_block_switch(statement_iterator: &mut StatementIterator) -> ASTBlock {
    parse_block(statement_iterator, &|token| match token.token_type {
        TokenType::Keyword(Keywords::OMG | Keywords::OMGWTF | Keywords::OIC) => true,
        _ => false,
    })
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::VariableAccess;
    use std::{collections::VecDeque, rc::Rc};

    use crate::{
        lexer::{Keywords, TokenType, TokenValue},
        parser::{
            expression::ASTExpression,
            statements::{assignment::VariableAssignment, i_has_a::IHasA, visible::Visible},
            Identifier,
        },
    };

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn block_root() {
        let block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::I_HAS_A),
                TokenType::Identifier("Batata".to_string()),
                TokenType::Keyword(Keywords::ITZ),
                TokenType::Value(TokenValue::NOOB),
            ],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::NOOB),
            ],
            vec![
                TokenType::Identifier("Batata".to_string()),
                TokenType::Keyword(Keywords::R),
                TokenType::Value(TokenValue::NOOB),
            ],
        ]);

        assert_eq!(
            parse_block_root(&mut block_tokens.clone().into()),
            ASTBlock::from([
                IHasA::from((
                    (block_tokens[0][1].clone(), false).into(),
                    (ASTExpression::LiteralValue(block_tokens[0][3].clone()))
                ))
                .into(),
                Visible(
                    VecDeque::from([ASTExpression::LiteralValue(block_tokens[1][1].clone())]),
                    None
                )
                .into(),
                VariableAssignment {
                    identifier: VariableAccess {
                        name: (block_tokens[2][0].clone(), false).into(),
                        accesses: [].into()
                    },
                    value: ASTExpression::LiteralValue(block_tokens[2][2].clone())
                }
                .into()
            ])
        )
    }

    #[test]
    fn block_function() {
        let block_tokens = Token::make_block(vec![
            vec![
                TokenType::Keyword(Keywords::I_HAS_A),
                TokenType::Identifier("Batata".to_string()),
            ],
            vec![
                TokenType::Identifier("Batata".to_string()),
                TokenType::Keyword(Keywords::R),
                TokenType::Keyword(Keywords::SUM_OF),
                TokenType::Identifier("Batata".to_string()),
                TokenType::Keyword(Keywords::AN),
                TokenType::Value(TokenValue::NOOB),
            ],
            vec![
                TokenType::Keyword(Keywords::FOUND_YR),
                TokenType::Identifier("Batata".to_string()),
            ],
            vec![TokenType::Keyword(Keywords::IF_U_SAY_SO)],
            vec![
                TokenType::Keyword(Keywords::VISIBLE),
                TokenType::Value(TokenValue::NOOB),
            ],
        ]);

        assert_eq!(
            parse_block_function(&mut block_tokens.clone().into()),
            ASTBlock::from([
                IHasA::from(Identifier::from((block_tokens[0][1].clone(), false))).into(),
                VariableAssignment {
                    identifier: VariableAccess {
                        name: (block_tokens[1][0].clone(), false).into(),
                        accesses: [].into()
                    },
                    value: ASTExpression::SumOf(
                        Rc::new(ASTExpression::VariableAccess(VariableAccess {
                            name: (block_tokens[1][3].clone(), false).into(),
                            accesses: [].into()
                        },)),
                        Rc::new(ASTExpression::LiteralValue(block_tokens[1][5].clone()))
                    )
                }
                .into(),
                ASTNode::FoundYr(ASTExpression::VariableAccess(VariableAccess {
                    name: (block_tokens[2][1].clone(), false).into(),
                    accesses: [].into()
                }))
            ])
        )
    }
}
