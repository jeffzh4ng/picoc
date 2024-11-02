use serde::{Deserialize, Serialize};
use std::iter;

#[rustfmt::skip]
#[derive(Copy, Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum TokenType {
    LiteralInt, Identifier, // introductions (values) RE: [0-9]+ and [a-zA-Z][a-zA-Z0-9]*
    KeywordInt, KeywordMain, KeywordVoid, KeywordRet, KeywordIf, KeywordEls, KeywordFor, // keywords ⊂ identifiers
    Plus, Minus, Star, Slash, LeftAngleBracket, RightAngleBracket, Equals, Bang, Amp, Bar, // eliminations (ops)
    PuncLeftParen, PuncRightParen, PuncLeftBrace, PuncRightBrace, PuncSemiColon, // punctuation
}

//  1. variations are explicitly typed. Collapsing categories like keywords
//     into one variant will lose information since lexeme : String, which
//     will produce redundant work for the parser during syntactic analysis
//  2. non-tokens: comments, preprocessor directives, macros, whitespace

#[rustfmt::skip]
#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct Token { pub lexeme: String, pub typ: TokenType }

// TODO: keep track of file and (col, row) for error reporting
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
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("+"), typ: TokenType::Plus };
                iter::once(t).chain(lex(r)).collect()
            }
            '-' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("-"), typ: TokenType::Minus };
                iter::once(t).chain(lex(r)).collect()
            }
            '*' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("*"), typ: TokenType::Star };
                iter::once(t).chain(lex(r)).collect()
            }
            '/' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("/"), typ: TokenType::Slash };
                iter::once(t).chain(lex(r)).collect()
            }
            '<' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("<"), typ: TokenType::LeftAngleBracket };
                iter::once(t).chain(lex(r)).collect()
            }
            '>' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from(">"), typ: TokenType::RightAngleBracket };
                iter::once(t).chain(lex(r)).collect()
            }
            '=' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("="), typ: TokenType::Equals };
                iter::once(t).chain(lex(r)).collect()
            }
            '!' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("!"), typ: TokenType::Bang };
                iter::once(t).chain(lex(r)).collect()
            }
            '&' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("&"), typ: TokenType::Amp };
                iter::once(t).chain(lex(r)).collect()
            }
            '|' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("|"), typ: TokenType::Bar };
                iter::once(t).chain(lex(r)).collect()
            }
            '(' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("("), typ: TokenType::PuncLeftParen };
                iter::once(t).chain(lex(r)).collect()
            }
            ')' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from(")"), typ: TokenType::PuncRightParen };
                iter::once(t).chain(lex(r)).collect()
            }
            '{' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("{"), typ: TokenType::PuncLeftBrace };
                iter::once(t).chain(lex(r)).collect()
            }
            '}' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("}"), typ: TokenType::PuncRightBrace };
                iter::once(t).chain(lex(r)).collect()
            }
            ';' => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from(";"), typ: TokenType::PuncSemiColon };
                iter::once(t).chain(lex(r)).collect()
            }
            _ => {
                #[rustfmt::skip]
                let t = Token { lexeme: String::from("PANIC?"), typ: TokenType::Plus };
                iter::once(t).chain(lex(r)).collect()
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

                iter::once(t).chain(lex(new_r)).collect()
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

                iter::once(t).chain(lex(new_r)).collect()
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
mod test_arith {
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/arith";

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
mod test_bindings {
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/bindings";

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

#[cfg(test)]
mod test_control {
    use std::fs;

    const TEST_DIR: &str = "tests/fixtures/control";

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
