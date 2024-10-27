use serde::{Deserialize, Serialize};

// non-tokens:
// - comments
// - preprocessor directives
// - macros
// - whitespace: spaces, tabs, newlines

// note: variations are explicitly typed. Collapsing categories like keywords
//       into one variant will lose information since lexeme : String, which
//       will produce redundant work for the parser during syntactic analysis
#[derive(Copy, Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum TokenType {
    // introductions (values)
    LiteralInt, // RE: [0-9]+
    Identifier, // RE: [a−zA−Z][a−zA−Z0−9]*

    // keywords (subset of identifiers)
    KeywordInt,
    KeywordMain,
    KeywordVoid,
    KeywordRet,
    KeywordIf,
    KeywordEls,
    KeywordFor,

    // eliminations (operations)
    Plus,
    Minus,
    Star,
    Slash,
    LeftAngleBracket,
    RightAngleBracket,
    Equals,
    Bang,
    Amp,
    Bar,

    // punctuation
    PuncLeftParen,
    PuncRightParen,
    PuncLeftBrace,
    PuncRightBrace,
    PuncSemiColon,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct Token {
    pub lexeme: String,
    pub typ: TokenType,
}

// TODO: keep track of file and (col, row) for error reporting
// struct Position {}

// TODO: just filter out whitespace instead of having a helper function
pub fn lex(input: &[char]) -> Vec<Token> {
    let cs = skip_whitespace(input);

    // literals and identifiers have arbitrary length
    // operations and punctuations are single ASCII characters
    match cs {
        [] => vec![],
        [f, r @ ..] => match f {
            '0'..='9' => scan_int(cs),
            'a'..='z' | 'A'..='Z' => scan_id(cs),
            '+' => {
                let t = Token {
                    lexeme: String::from("+"),
                    typ: TokenType::Plus,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '-' => {
                let t = Token {
                    lexeme: String::from("-"),
                    typ: TokenType::Minus,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '*' => {
                let t = Token {
                    lexeme: String::from("*"),
                    typ: TokenType::Star,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '/' => {
                let t = Token {
                    lexeme: String::from("/"),
                    typ: TokenType::Slash,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '<' => {
                let t = Token {
                    lexeme: String::from("<"),
                    typ: TokenType::LeftAngleBracket,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '>' => {
                let t = Token {
                    lexeme: String::from(">"),
                    typ: TokenType::RightAngleBracket,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '=' => {
                let t = Token {
                    lexeme: String::from("="),
                    typ: TokenType::Equals,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '!' => {
                let t = Token {
                    lexeme: String::from("!"),
                    typ: TokenType::Bang,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '&' => {
                let t = Token {
                    lexeme: String::from("&"),
                    typ: TokenType::Amp,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '|' => {
                let t = Token {
                    lexeme: String::from("|"),
                    typ: TokenType::Bar,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '(' => {
                let t = Token {
                    lexeme: String::from("("),
                    typ: TokenType::PuncLeftParen,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            ')' => {
                let t = Token {
                    lexeme: String::from(")"),
                    typ: TokenType::PuncRightParen,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '{' => {
                let t = Token {
                    lexeme: String::from("{"),
                    typ: TokenType::PuncLeftBrace,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            '}' => {
                let t = Token {
                    lexeme: String::from("}"),
                    typ: TokenType::PuncRightBrace,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            ';' => {
                let t = Token {
                    lexeme: String::from(";"),
                    typ: TokenType::PuncSemiColon,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
            _ => {
                let t = Token {
                    lexeme: String::from("PANIC?"),
                    typ: TokenType::Plus,
                };

                std::iter::once(t).chain(lex(r)).collect()
            }
        },
    }
}

fn scan_int(input: &[char]) -> Vec<Token> {
    // scan_int calls skip_whitespace too to remain idempotent
    let cs = skip_whitespace(input);

    match cs {
        [] => vec![],
        [f, r @ ..] => match f {
            '0'..='9' => {
                #[rustfmt::skip]
                let i = r
                    .iter()
                    .take_while(|&&c| c.is_numeric())
                    .count();

                #[rustfmt::skip]
                let f = cs[..=i]
                    .iter()
                    .collect::<String>();
                let new_r = &cs[i + 1..];

                let t = Token {
                    lexeme: f,
                    typ: TokenType::LiteralInt,
                };

                std::iter::once(t).chain(lex(new_r)).collect()
            }
            _ => {
                // panic
                todo!()
            }
        },
    }
}

// TODO: support identifiers with alpha*numeric* characters after first alphabetic
fn scan_id(input: &[char]) -> Vec<Token> {
    // scan_id calls skip_whitespace too to remain idempotent
    let cs = skip_whitespace(input);

    match cs {
        [] => vec![],
        [f, r @ ..] => match f {
            'a'..='z' => {
                // Find the index where the alphabetic characters end
                #[rustfmt::skip]
                let i = r
                    .iter()
                    .take_while(|&&c| c.is_alphabetic())
                    .count();

                let f = (cs[..=i].iter()).collect::<String>();
                let new_r = &cs[i + 1..];

                let keyword = match f.as_str() {
                    "int" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TokenType::KeywordInt,
                    }),
                    "main" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TokenType::KeywordMain,
                    }),
                    "if" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TokenType::KeywordIf,
                    }),
                    "else" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TokenType::KeywordEls,
                    }),
                    "for" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TokenType::KeywordFor,
                    }),
                    "return" => Some(Token {
                        lexeme: f.to_string(),
                        typ: TokenType::KeywordRet,
                    }),
                    _ => None,
                };

                let t = match keyword {
                    Some(k) => k,
                    None => Token {
                        lexeme: f,
                        typ: TokenType::Identifier,
                    },
                };

                std::iter::once(t).chain(lex(new_r)).collect()
            }
            _ => {
                // panic
                todo!()
            }
        },
    }
}

fn skip_whitespace(input: &[char]) -> &[char] {
    match input {
        [] => input,
        [f, r @ ..] => {
            if f.is_whitespace() {
                skip_whitespace(r)
            } else {
                input
            }
        }
    }
}

#[cfg(test)]
mod test_legal_arithmetic {
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/legal/arithmetic";

    #[test]
    fn lit() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/lit.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice());
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: KeywordMain
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "8"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn add() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/add.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice());
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: KeywordMain
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: +
          typ: Plus
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn add_multi() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/add_multi.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice());
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: KeywordMain
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: +
          typ: Plus
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: +
          typ: Plus
        - lexeme: "11"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn sub() {
        #[rustfmt::skip]
        let input = fs::read(format!("{}/sub.c", TEST_DIR))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice());
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: KeywordMain
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "88"
          typ: LiteralInt
        - lexeme: "-"
          typ: Minus
        - lexeme: "32"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn mult() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/mult.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice());
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: KeywordMain
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: "*"
          typ: Star
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }

    #[test]
    fn div() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/div.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice());
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: KeywordMain
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: "100"
          typ: LiteralInt
        - lexeme: /
          typ: Slash
        - lexeme: "9"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }
}

#[cfg(test)]
mod test_legal_control_flow {
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/legal/control_flow";

    #[test]
    fn for_loop() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/for.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice());
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: KeywordMain
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: int
          typ: KeywordInt
        - lexeme: n
          typ: Identifier
        - lexeme: "="
          typ: Equals
        - lexeme: "0"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: for
          typ: KeywordFor
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: int
          typ: KeywordInt
        - lexeme: i
          typ: Identifier
        - lexeme: "="
          typ: Equals
        - lexeme: "0"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: i
          typ: Identifier
        - lexeme: "<"
          typ: LeftAngleBracket
        - lexeme: "10"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: i
          typ: Identifier
        - lexeme: +
          typ: Plus
        - lexeme: +
          typ: Plus
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: n
          typ: Identifier
        - lexeme: +
          typ: Plus
        - lexeme: "="
          typ: Equals
        - lexeme: "1"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: n
          typ: Identifier
        - lexeme: +
          typ: Plus
        - lexeme: "="
          typ: Equals
        - lexeme: "1"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        - lexeme: return
          typ: KeywordRet
        - lexeme: n
          typ: Identifier
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }
}

#[cfg(test)]
mod test_legal_data_flow {
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/legal/data_flow";

    #[test]
    fn asnmt() {
        #[rustfmt::skip]
        let input = fs::read(format!("{TEST_DIR}/asnmt.c"))
            .expect("Should have been able to read the file")
            .iter()
            .map(|b| *b as char)
            .collect::<Vec<_>>();

        let output = super::lex(input.as_slice());
        insta::assert_yaml_snapshot!(output, @r###"
        ---
        - lexeme: int
          typ: KeywordInt
        - lexeme: main
          typ: KeywordMain
        - lexeme: (
          typ: PuncLeftParen
        - lexeme: )
          typ: PuncRightParen
        - lexeme: "{"
          typ: PuncLeftBrace
        - lexeme: int
          typ: KeywordInt
        - lexeme: x
          typ: Identifier
        - lexeme: "="
          typ: Equals
        - lexeme: "8"
          typ: LiteralInt
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: return
          typ: KeywordRet
        - lexeme: x
          typ: Identifier
        - lexeme: ;
          typ: PuncSemiColon
        - lexeme: "}"
          typ: PuncRightBrace
        "###);
    }
}

// #[cfg(test)]
// fn vecs_match<T: PartialEq>(a: &Vec<T>, b: &Vec<T>) -> bool {
//     #[rustfmt::skip]
//     let matching = a
//         .iter()
//         .zip(b.iter())
//         .filter(|&(a, b)| a == b)
//         .count();

//     matching == a.len() && matching == b.len()
// }

// #[cfg(test)]
// mod test_invalid {}

// #[cfg(test)]
// mod test_skip_whitespace {
//     use super::*;

//     #[test]
//     fn skip_space() {
//         let input = "    7".chars().collect::<Vec<_>>();
//         let output = skip_whitespace(input.as_slice());
//         let expected_output = "7".chars().collect::<Vec<_>>();

//         assert!(vecs_match(&output.to_vec(), &expected_output))
//     }

//     #[test]
//     fn skip_newline() {
//         let input = r#"

//         7"#
//         .chars()
//         .collect::<Vec<_>>();
//         let output = skip_whitespace(input.as_slice());
//         let expected_output = "7".chars().collect::<Vec<_>>();

//         assert!(vecs_match(&output.to_vec(), &expected_output))
//     }
// }
