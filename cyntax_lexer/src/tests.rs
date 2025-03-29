use std::ops::Range;

use crate::{Punctuator, Token, Whitespace, lexer::Lexer};
fn test_helper(source: &str, ranges: &[Range<usize>]) -> String {
    let mut s = String::new();
    for range in ranges {
        s.push_str(&source[range.clone()]);
    }
    s
}

#[test]
fn test_lex_number_basic() {
    let source = "123 45.6 7.89e+10";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "45.6".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "7.89e+10".to_string()
    );
}

#[test]
fn test_lex_number_torture() {
    let source = "5.15e+5testee+5 0.1.2.3e-4abc123e+10";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "5.15e+5testee+5".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "0.1.2.3e-4abc123e+10".to_string()
    );
}

#[test]
fn test_lex_number_leading_dot() {
    let source = ".123 .456e-7";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        ".123".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        ".456e-7".to_string()
    );
}
#[test]
fn test_lex_number_ending_e() {
    let source = "123e 123E 123p 123P";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123e".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123E".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123p".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123P".to_string()
    );
}

#[test]
fn test_lex_number_ending_e_plus() {
    let source = "123e+ 123E+ 123p+ 123P+";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123e+".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123E+".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123p+".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123P+".to_string()
    );
}

#[test]
fn test_lex_number_ending_e_minus() {
    let source = "123e- 123E- 123p- 123P-";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123e-".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123E-".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123p-".to_string()
    );
    assert_eq!(
        test_helper(
            source,
            &match skip_whitespace(&mut lexer).unwrap().1 {
                Token::PPNumber(ranges) => ranges,
                _ => panic!(),
            }
        ),
        "123P-".to_string()
    );
}

fn skip_whitespace(lexer: &mut Lexer) -> Option<(Range<usize>, Token)> {
    loop {
        match lexer.next() {
            Some((_, Token::Whitespace(_))) => continue,
            other => return other,
        }
    }
}
#[test]
fn test_empty_string_literal() {
    let source = "\"\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_simple_string_literal() {
    let source = "\"hello\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_spaces() {
    let source = "\"hello world\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello world".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_escaped_quote() {
    let source = "\"hello\\\"world\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello\"world".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_multiple_escaped_quotes() {
    let source = "\"hello\\\"world\\\"test\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello\"world\"test".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_other_chars() {
    let source = "\"hello123world!@#\"";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello123world!@#".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_identifier_after() {
    let source = "\"hello\"world";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Identifier(_))
    ));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_identifier_before() {
    let source = "world\"hello\"";
    let mut lexer = Lexer::new(source);
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Identifier(_))
    ));
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_punctuator_after() {
    let source = "\"hello\";";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Punctuator(Punctuator::Semicolon))
    ));
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_punctuator_before() {
    let source = ";\"hello\"";
    let mut lexer = Lexer::new(source);
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Punctuator(Punctuator::Semicolon))
    ));
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_whitespace_before() {
    let source = " \"hello\"";
    let mut lexer = Lexer::new(source);
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Whitespace(Whitespace::Space))
    ));
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn test_string_literal_with_whitespace_after() {
    let source = "\"hello\" ";
    let mut lexer = Lexer::new(source);
    assert_eq!(
        lexer.next().map(|t| match t.1 {
            Token::StringLiteral(ranges) => test_helper(source, &ranges),
            _ => panic!("Expected StringLiteral"),
        }),
        Some("hello".to_string())
    );
    assert!(matches!(
        lexer.next().map(|t| t.1),
        Some(Token::Whitespace(Whitespace::Space))
    ));
    assert_eq!(lexer.next(), None);
}
