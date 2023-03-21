use crate::parser::error::ASTErrorType;
use crate::parser::statements::Node;
use std::collections::VecDeque;

use crate::parser::StatementIterator;

use crate::lexer::{Keywords, Token, TokenType};

use super::parse_statement;
use super::statements::how_is_i::HowIzI;
use super::statements::im_in_yr::ImInYr;
use super::statements::o_rly::ORly;
use super::statements::wtf::Wtf;

/// A block of code, which is an array of statements
#[derive(Debug, PartialEq, Clone)]
pub struct ASTBlock(pub VecDeque<Node>);

impl Default for ASTBlock {
    fn default() -> Self {
        Self(VecDeque::new())
    }
}

impl<const T: usize> From<[Node; T]> for ASTBlock {
    fn from(value: [Node; T]) -> Self {
        Self(VecDeque::from(value))
    }
}

struct ASTBlockErrorIterator<'a> {
    blocks: Vec<&'a ASTBlock>,
    current_block_index: usize,
    current_block_statement: usize,
}

impl<'a> ASTBlockErrorIterator<'a> {
    pub fn new(first_block: &'a ASTBlock) -> ASTBlockErrorIterator<'a> {
        ASTBlockErrorIterator {
            blocks: vec![first_block],
            current_block_index: 0,
            current_block_statement: 0,
        }
    }
}

impl Node {
    pub fn get_inner_block(&self) -> Option<Vec<&ASTBlock>> {
        match self {
            Node::Wtf(Wtf { omg, omg_wtf }) => {
                let mut vec: Vec<&ASTBlock> = omg.iter().map(|(_, block)| block).collect();
                omg_wtf.as_ref().map(|block| vec.push(block));
                Some(vec)
            }
            Node::ORly(ORly {
                if_true,
                if_false,
                mebbes,
            }) => {
                let mut vec: Vec<&ASTBlock> = vec![];
                if_true.as_ref().map(|block| vec.push(block));
                mebbes.iter().for_each(|(_, block)| vec.push(block));
                if_false.as_ref().map(|block| vec.push(block));
                Some(vec)
            }
            Node::ImInYr(ImInYr { code_block, .. }) => Some(vec![code_block]),
            Node::HowIzI(HowIzI { body, .. }) => Some(vec![body]),
            _ => None,
        }
    }
}

enum ASTErrorOrBlock<'a> {
    Error(&'a ASTErrorType),
    Block(Vec<&'a ASTBlock>),
}

impl<'a> Iterator for ASTBlockErrorIterator<'a> {
    type Item = ASTErrorType;

    fn next(&mut self) -> Option<Self::Item> {
        let _block_node = self
            .blocks
            .iter()
            .skip(self.current_block_index)
            .find_map(|block| {
                block
                    .0
                    .iter()
                    .skip(self.current_block_statement)
                    .find_map(|node| {
                        self.current_block_statement += 1;
                        node.get_inner_block()
                            .map(|block| ASTErrorOrBlock::Block(block))
                            .or_else(|| match node {
                                Node::ASTError(error) => Some(ASTErrorOrBlock::Error(error)),
                                _ => None,
                            })
                    })
                    .or_else(|| {
                        self.current_block_index += 1;
                        None
                    })
            });

        todo!()
    }
}

impl ASTBlock {
    pub fn iter_errors(&self) -> impl Iterator + '_ {
        ASTBlockErrorIterator::new(self)
    }
}

impl IntoIterator for ASTBlock {
    type Item = Node;
    type IntoIter = std::collections::vec_deque::IntoIter<Node>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn parse_block(
    statement_iterator: &mut StatementIterator,
    should_stop_at_token: &dyn Fn(&Token) -> bool,
) -> ASTBlock {
    let mut block = ASTBlock::default();
    loop {
        match statement_iterator.next() {
            Some(token) => block.0.push_back(
                parse_statement(token, statement_iterator).unwrap_or_else(|err| err.into()),
            ),
            None => {}
        };

        statement_iterator
            .next_statement_should_be_empty()
            .unwrap_or_else(|err| block.0.push_back(err.into()));

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
    use crate::parser::{
        expression::{ASTExpressionValue, BinaryOperation, BinaryOpt, VariableAccess},
        statements::{found_yr::FoundYr, i_has_a::IHasAInitialValue},
    };
    use std::collections::VecDeque;

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
                IHasA {
                    i_has_a_token: block_tokens[0][0].clone(),
                    identifier: Identifier {
                        name: block_tokens[0][1].clone(),
                        srs: None,
                    },
                    initial_value: Some(IHasAInitialValue::Expression(ASTExpression::Value(
                        ASTExpressionValue::LiteralValue(block_tokens[0][3].clone())
                    )))
                }
                .into(),
                Visible(
                    VecDeque::from([ASTExpression::Value(ASTExpressionValue::LiteralValue(
                        block_tokens[1][1].clone()
                    ))]),
                    None
                )
                .into(),
                VariableAssignment {
                    variable_access: VariableAccess {
                        identifier: Identifier {
                            name: block_tokens[2][0].clone(),
                            srs: None
                        },
                        accesses: VecDeque::new()
                    },
                    r_token: block_tokens[2][1].clone(),
                    expression: ASTExpression::Value(ASTExpressionValue::LiteralValue(
                        block_tokens[2][2].clone()
                    ))
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
                Node::IHasA(IHasA {
                    i_has_a_token: block_tokens[0][0].clone(),
                    identifier: Identifier {
                        name: block_tokens[0][1].clone(),
                        srs: None
                    },
                    initial_value: None,
                }),
                VariableAssignment {
                    variable_access: VariableAccess {
                        identifier: Identifier {
                            name: block_tokens[1][0].clone(),
                            srs: None
                        },
                        accesses: VecDeque::new()
                    },
                    r_token: block_tokens[1][1].clone(),
                    expression: ASTExpression::BinaryOperation(BinaryOperation {
                        operator: BinaryOpt::SumOf(block_tokens[1][2].clone()),
                        left: Box::new(ASTExpression::Value(ASTExpressionValue::VariableAccess(
                            VariableAccess {
                                identifier: Identifier {
                                    name: block_tokens[1][3].clone(),
                                    srs: None
                                },
                                accesses: VecDeque::new()
                            }
                        ))),
                        an_token: Some(block_tokens[1][4].clone()),
                        right: Box::new(ASTExpression::Value(ASTExpressionValue::LiteralValue(
                            block_tokens[1][5].clone()
                        )))
                    })
                }
                .into(),
                Node::FoundYr(FoundYr {
                    found_yr_token: block_tokens[2][0].clone(),
                    expression: ASTExpression::Value(ASTExpressionValue::VariableAccess(
                        VariableAccess {
                            identifier: Identifier {
                                name: block_tokens[2][1].clone(),
                                srs: None,
                            },
                            accesses: VecDeque::new()
                        }
                    ))
                })
            ])
        )
    }
}
