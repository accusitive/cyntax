use std::{ops::Range, str::Chars};

use cyntax_common::spanned::{Location, Spanned};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;
use peekmore::{PeekMore, PeekMoreIterator};

use crate::PResult;

#[derive(Debug, PartialEq, Eq)]
pub enum Stage {
    Prefix,
    Number,
    Suffix,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Signedness {
    Unsigned,
    None,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Width {
    Long,
    LongLong,
    None,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Suffix {
    signed: Signedness,
    width: Width,
}
#[derive(Debug, PartialEq, Clone)]
pub struct IntConstant {
    number: String,
    suffix: Suffix,
}
pub struct ConstantLexer<'a> {
    last_location: Location,
    chars: PeekMoreIterator<Chars<'a>>,
    number_part: String,
    base: u8,
    stage: Stage,
    suffix: Suffix,
}
impl<'a> ConstantLexer<'a> {
    pub fn new(span: Location, number: &'a str) -> ConstantLexer<'a> {
        ConstantLexer {
            last_location: span,
            chars: number.chars().peekmore(),
            number_part: String::new(),
            base: 10,
            stage: Stage::Prefix,
            suffix: Suffix { signed: Signedness::None, width: Width::None },
        }
    }
    pub fn lex(mut self) -> PResult<IntConstant> {
        while let Some(_) = self.chars.peek() {
            self.handle_next_char()?
        }
        Ok(IntConstant { number: self.number_part, suffix: self.suffix })
    }
    fn next_char(&mut self) -> Option<Spanned<char>> {
        let value = self.chars.next()?;
        let l = value.len_utf8();
        let range = self.last_location.until(&Location { range: self.last_location.range.start..self.last_location.range.end+l, file_id: self.last_location.file_id });
        self.last_location = Location { range: self.last_location.range.start+l..self.last_location.range.end+l, file_id: self.last_location.file_id };

        Some(Spanned::new(range, value))
    }
    pub fn handle_next_char(&mut self) -> PResult<()> {
        match self.next_char() {
            Some(span!('0')) if self.stage == Stage::Prefix => {
                if matches!(self.chars.peek(), Some('x' | 'X')) {
                    self.next_char();
                    self.base = 16;
                    self.stage = Stage::Number;
                } else {
                    self.number_part.push('0');
                    self.base = 8;
                    self.stage = Stage::Number;
                }
            }
            Some(span!(c @ ('0'..='9' | 'a'..='f' | 'A'..='F') )) if self.base == 16 && matches!(self.stage, Stage::Number | Stage::Prefix) => {
                self.stage = Stage::Number;
                self.number_part.push(c);
            }
            Some(span!(c @ '0'..='9')) if self.base == 10 && matches!(self.stage, Stage::Number | Stage::Prefix) => {
                self.stage = Stage::Number;
                self.number_part.push(c);
            }
            Some(span!(c @ '0'..='7')) if self.base == 8 && matches!(self.stage, Stage::Number | Stage::Prefix) => {
                self.stage = Stage::Number;
                self.number_part.push(c);
            }
            Some(span!('u' | 'U')) if matches!(self.stage, Stage::Number | Stage::Suffix) => {
                self.stage = Stage::Suffix;
                self.suffix.signed = Signedness::Unsigned;
            }
            Some(span!(s, 'l' | 'L')) if matches!(self.stage, Stage::Number | Stage::Suffix) => {
                self.stage = Stage::Suffix;
                match self.suffix.width {
                    Width::None => self.suffix.width = Width::Long,
                    Width::Long => self.suffix.width = Width::LongLong,
                    Width::LongLong => return Err(SimpleError(s, "expected maximum of two suffix width specifiers".to_string()).into_codespan_report()),
                }
            }

            Some(span!(s,  c @('8' | '9'))) if self.base == 8 => return Err(SimpleError(s, format!("invalid character {c} for base {} constant", self.base)).into_codespan_report()),
            _ => return Err(SimpleError(self.last_location.clone(), "unhandled".to_string()).into_codespan_report()),
        };

        Ok(())
    }
}
