use lexer::TokenError;

pub mod lexer;
pub mod parser;

pub use lexer::tokenize;
pub use parser::parse;

pub fn tokenize_and_parse(code: String) -> Result<parser::ASTBlock, TokenError> {
    let tokens = tokenize(code)?;
    Ok(parse(tokens))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    #[test]
    fn print_even_numbers() {
        let code = r#"I HAS A N ITZ 0

IM IN YR LOOP UPPIN YR N TIL BOTH SAEM N AN 10
	BOTH SAEM 0 AN QUOSHUNT OF N AN 2, O RLY?
		YA RLY
			VISIBLE SMOOSH N AN ", " MKAY!
	OIC
	VISIBLE
IM OUTTA YR LOOP

VISIBLE "Program ended"
"#
        .to_string();

        let ast = super::tokenize_and_parse(code).unwrap();

        assert_eq!(
            format!("{:#?}", ast),
            r#"ASTBlock(
    [
        IHasA(
            IHasA {
                identifier: Identifier {
                    name: (1, 9) -> (1, 10)
                    Identifier("N")
                    ,
                    is_srs: false,
                },
                initial_value: Some(
                    Expression(
                        LiteralValue(
                            (1, 15) -> (1, 16)
                            Value(Number(Int(0)))
                            ,
                        ),
                    ),
                ),
            },
        ),
        ImInYr(
            ImInYr {
                label: (3, 10) -> (3, 14)
                Identifier("LOOP")
                ,
                on_iteration: Some(
                    LoopIterationOperation {
                        operation: UPPIN(
                            (3, 15) -> (3, 20)
                            Keyword(UPPIN)
                            ,
                        ),
                        operand: VariableAccess {
                            name: Identifier {
                                name: (3, 24) -> (3, 25)
                                Identifier("N")
                                ,
                                is_srs: false,
                            },
                            accesses: [],
                        },
                        condition: Some(
                            TIL(
                                BothSaem(
                                    VariableAccess(
                                        VariableAccess {
                                            name: Identifier {
                                                name: (3, 40) -> (3, 41)
                                                Identifier("N")
                                                ,
                                                is_srs: false,
                                            },
                                            accesses: [],
                                        },
                                    ),
                                    LiteralValue(
                                        (3, 45) -> (3, 47)
                                        Value(Number(Int(10)))
                                        ,
                                    ),
                                ),
                            ),
                        ),
                    },
                ),
                code_block: ASTBlock(
                    [
                        Expression(
                            BothSaem(
                                LiteralValue(
                                    (4, 12) -> (4, 13)
                                    Value(Number(Int(0)))
                                    ,
                                ),
                                QuoshuntOf(
                                    VariableAccess(
                                        VariableAccess {
                                            name: Identifier {
                                                name: (4, 29) -> (4, 30)
                                                Identifier("N")
                                                ,
                                                is_srs: false,
                                            },
                                            accesses: [],
                                        },
                                    ),
                                    LiteralValue(
                                        (4, 34) -> (4, 35)
                                        Value(Number(Int(2)))
                                        ,
                                    ),
                                ),
                            ),
                        ),
                        ORly(
                            ORly {
                                if_true: Some(
                                    ASTBlock(
                                        [
                                            Visible(
                                                Visible(
                                                    [
                                                        Smoosh(
                                                            [
                                                                VariableAccess(
                                                                    VariableAccess {
                                                                        name: Identifier {
                                                                            name: (6, 19) -> (6, 20)
                                                                            Identifier("N")
                                                                            ,
                                                                            is_srs: false,
                                                                        },
                                                                        accesses: [],
                                                                    },
                                                                ),
                                                                LiteralValue(
                                                                    (6, 24) -> (6, 28)
                                                                    Value(String(", "))
                                                                    ,
                                                                ),
                                                            ],
                                                        ),
                                                    ],
                                                    Some(
                                                        (6, 33) -> (6, 34)
                                                        ExclamationMark
                                                        ,
                                                    ),
                                                ),
                                            ),
                                        ],
                                    ),
                                ),
                                if_false: None,
                                mebbes: [],
                            },
                        ),
                        Visible(
                            Visible(
                                [],
                                None,
                            ),
                        ),
                    ],
                ),
                end_label: (9, 13) -> (9, 17)
                Identifier("LOOP")
                ,
            },
        ),
        Visible(
            Visible(
                [
                    LiteralValue(
                        (11, 9) -> (11, 24)
                        Value(String("Program ended"))
                        ,
                    ),
                ],
                None,
            ),
        ),
    ],
)"#
        )
    }
}
