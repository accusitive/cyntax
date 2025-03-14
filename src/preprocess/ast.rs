use codespan_reporting::diagnostic::{Diagnostic, Label};
use peekmore::PeekMore;

use crate::{lexer::Punctuator, location::LocationHistory, parser::ast::Keyword};

use super::PPResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Constant(Constant),
    StringLiteral(String),
    Punctuator(Punctuator),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Integer(IntConstant),
    Float(FloatConstant),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntegerConstantBase {
    /// example: 052
    Octal,
    // example: 42
    Decimal,
    /// example: 0x2a
    Hexadecimal,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntegerSuffixLength {
    /// 42l, 42L
    Long,
    /// 42ll, 42LL
    LongLong,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerSuffix {
    /// U
    unsigned: bool,
    length: Option<IntegerSuffixLength>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntConstant {
    pub base: IntegerConstantBase,
    pub suffix: Option<IntegerSuffix>,
    /// the actual digit component of the constant
    /// 0x15A0e5
    ///   ^^^^
    pub digits: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FloatConstantBase {
    /// example: 3.14, 1e3
    Decimal,
    /// example: 0x1.2p3
    Hexadecimal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FloatSuffix {
    /// 'f' or 'F'
    Float,
    /// 'l' or 'L'
    LongDouble,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FloatConstant {
    base: FloatConstantBase,
    digits: String,
    exponent: Option<String>,
    suffix: Option<FloatSuffix>,
}
impl FloatConstant {
    pub fn parse(_: &LocationHistory<String>) -> PPResult<Self> {
        todo!()
    }
}

impl IntConstant {
    pub fn parse(s: &LocationHistory<String>) -> PPResult<Self> {
        let mut chars = s.value.chars().peekmore();
        let first = match chars.peek() {
            Some(first) => *first,
            None => return Err(Diagnostic::bug().with_message("Trying to parse empty constant?").with_labels(s.generate_location_labels())),
        };

        let (base, radix) = match first {
            '0' if chars.peek_nth(1) == Some(&'x') || chars.peek_nth(1) == Some(&'X') => {
                chars.next().unwrap();
                chars.next().unwrap();
                (IntegerConstantBase::Hexadecimal, 16)
            }
            '0' => {
                chars.next().unwrap();
                (IntegerConstantBase::Octal, 8)
            }
            _ => (IntegerConstantBase::Decimal, 10),
        };

        let mut digits = String::new();
        while let Some(&c) = chars.peek() {
            if c.is_digit(radix) {
                digits.push(chars.next().unwrap());
            } else {
                break;
            }
        }

        // handle case `0`, this system parses it as an octal constant with no digits
        if digits.len() == 0 && matches!(base, IntegerConstantBase::Octal) {
            digits.push('0');
        }

        if digits.len() == 0 {
            let range = (s.location.start.offset + chars.cursor())..s.location.end.offset;
            return Err(Diagnostic::error().with_message("No digits").with_labels(vec![Label::primary(0, range)]));
        }
        let mut suffix = IntegerSuffix { unsigned: false, length: None };
        let mut suffixes = 0;
        let range = (s.location.start.offset + chars.cursor())..s.location.end.offset;

        while let Some(&c) = chars.peek() {
            match c {
                _ if suffixes > 2 => {
                    return Err(Diagnostic::error()
                        .with_message("Too many suffixes in constant")
                        .with_labels(vec![Label::primary(0, range)]));
                }

                'u' | 'U' => {
                    chars.next().unwrap();
                    suffix.unsigned = true;
                    suffixes += 1;
                }
                c @ ('l' | 'L') if chars.peek_nth(1) == Some(&c) => {
                    chars.next().unwrap();
                    chars.next().unwrap();
                    suffix.length = Some(IntegerSuffixLength::LongLong);
                    suffixes += 1;
                }
                'l' | 'L' => {
                    chars.next().unwrap();
                    suffix.length = Some(IntegerSuffixLength::Long);
                    suffixes += 1;
                }

                _ => {
                    return Err(Diagnostic::error()
                        .with_message("Invalid character in integer suffix")
                        .with_labels(vec![Label::primary(0, range)]));
                }
            }
        }

        Ok(IntConstant {
            base,
            digits,
            suffix: Some(suffix),
        })
    }
}
#[test]
fn test_suffixes() {
    // Unsigned suffix only
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123u".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix { unsigned: true, length: None }))
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123U".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix { unsigned: true, length: None }))
    );

    // Long suffix only
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123l".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix {
            unsigned: false,
            length: Some(IntegerSuffixLength::Long)
        }))
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123L".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix {
            unsigned: false,
            length: Some(IntegerSuffixLength::Long)
        }))
    );

    // Long Long suffix only
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123ll".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix {
            unsigned: false,
            length: Some(IntegerSuffixLength::LongLong)
        }))
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123LL".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix {
            unsigned: false,
            length: Some(IntegerSuffixLength::LongLong)
        }))
    );

    // Unsigned + Long
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123ul".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix {
            unsigned: true,
            length: Some(IntegerSuffixLength::Long)
        }))
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123Ul".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix {
            unsigned: true,
            length: Some(IntegerSuffixLength::Long)
        }))
    );

    // Unsigned + Long Long
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123ull".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix {
            unsigned: true,
            length: Some(IntegerSuffixLength::LongLong)
        }))
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123ULL".to_string())).map(|con| con.suffix),
        Ok(Some(IntegerSuffix {
            unsigned: true,
            length: Some(IntegerSuffixLength::LongLong)
        }))
    );

    // Invalid combinations
    assert!(IntConstant::parse(&LocationHistory::x("123xx".to_string())).is_err());
    assert!(IntConstant::parse(&LocationHistory::x("123llullululululu".to_string())).is_err());

    // Invalid suffix
}

#[test]
fn test_digits() {
    assert_eq!(IntConstant::parse(&LocationHistory::x("123".to_string())).map(|con| con.digits), Ok("123".to_string()));
    assert_eq!(IntConstant::parse(&LocationHistory::x("0x123".to_string())).map(|con| con.digits), Ok("123".to_string()));
    assert_eq!(IntConstant::parse(&LocationHistory::x("0123".to_string())).map(|con| con.digits), Ok("123".to_string()));
    assert!(IntConstant::parse(&LocationHistory::x("0".to_string())).is_ok());
    assert!(IntConstant::parse(&LocationHistory::x("".to_string())).is_err());
    assert!(IntConstant::parse(&LocationHistory::x("0x".to_string())).is_err());
    assert!(IntConstant::parse(&LocationHistory::x("ll".to_string())).is_err());
    assert!(IntConstant::parse(&LocationHistory::x("u".to_string())).is_err());
}

#[test]
fn test_base() {
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("01".to_string())).map(|con| con.base),
        Ok(IntegerConstantBase::Octal)
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("0x1".to_string())).map(|con| con.base),
        Ok(IntegerConstantBase::Hexadecimal)
    );

    assert_eq!(
        IntConstant::parse(&LocationHistory::x("1".to_string())).map(|con| con.base),
        Ok(IntegerConstantBase::Decimal)
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("2".to_string())).map(|con| con.base),
        Ok(IntegerConstantBase::Decimal)
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("123".to_string())).map(|con| con.base),
        Ok(IntegerConstantBase::Decimal)
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("0123".to_string())).map(|con| con.base),
        Ok(IntegerConstantBase::Octal)
    );
    assert_eq!(
        IntConstant::parse(&LocationHistory::x("0x123A".to_string())).map(|con| con.base),
        Ok(IntegerConstantBase::Hexadecimal)
    );
}

impl Token {
    pub fn describe<'a>(&self) -> String {
        match self {
            Token::Keyword(keyword) => keyword.as_str().to_string(),
            Token::Identifier(_) => "identifier".to_string(),
            Token::Constant(_) => "constant".to_string(),
            Token::StringLiteral(_) => "string literal".to_string(),
            Token::Punctuator(p) => format!("punctuation {:?}", p),
            // Token::Number(_) => "number".to_string(),
        }
    }

    pub fn as_identifier(&self) -> Option<&String> {
        if let Self::Identifier(v) = self {
            Some(v)
        } else {
            None
        }
    }

    // pub fn as_number(&self) -> Option<&Constant> {
    //     if let Self::Number(v) = self {
    //         Some(v)
    //     } else {
    //         None
    //     }
    // }

    pub fn as_constant(&self) -> Option<&Constant> {
        if let Self::Constant(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
