use crate::location::{Located, Location};
use peekmore::{PeekMore, PeekMoreIterator};
use std::str::Chars;

#[derive(Debug, Clone)]
enum DirectiveState {
    /// We have encountered a # but not yet a <
    InDirective,

    /// We are both inside of a directive, and inside angled brackets within that directive
    InInclude,
    /// guess
    InHeaderName,
    Out,
}
/// Main lexer state. Can only lex 1 file per instance.
#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    // location_offset: usize,
    location: Location,
    /// Did we just encounter a #?
    /// Used for parsing `<header-name.h>`
    directive_state: DirectiveState,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PreprocessingToken {
    HeaderName(String, HeaderNameKind),
    Identifier(String),
    Number(String),
    CharacterConstant,
    StringLiteral(String),
    Punctuator(Punctuator),
    Whitespace(char),
    Error(char),
    /// Used for pre processor to know when the directive ends
    Newline,
}
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Punctuator {
    LBracket,
    RBracket,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Dot,
    /// ->
    Arrow,
    PlusPlus,
    MinusMinus,
    /// &
    And,
    Asterisk,
    Plus,
    Minus,
    /// ~
    Tilde,
    /// !
    Bang,
    Slash,
    /// %
    Percent,
    ShiftLeft,
    ShiftRight,
    LeftAngle,
    RightAngle,

    LessOrEqual,
    GreatorOrEqual,

    // ==
    EqualEqual,
    /// !=
    NotEqual,

    /// ^
    Caret,
    /// |
    Or,
    AndAnd,
    OrOr,
    Question,
    Colon,
    Semicolon,
    DotDotDot,
    Equal,
    AsteriskEqual,
    SlashEqual,
    PercentEqual,
    PlusEqual,
    MinusEqual,
    /// <<=
    ShiftLeftEqual,
    /// >>=
    ShiftRightEqual,
    /// &=
    AndEqual,
    CaretEqual,
    /// |=
    OrEqual,
    Comma,
    /// #
    Hash,
    HashHash,
}
impl Punctuator {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '(' => Some(Punctuator::LParen),
            ')' => Some(Punctuator::RParen),
            '[' => Some(Punctuator::LBracket),
            ']' => Some(Punctuator::RBracket),
            '{' => Some(Punctuator::LBrace),
            '}' => Some(Punctuator::RBrace),
            '.' => Some(Punctuator::Dot),
            '+' => Some(Punctuator::Plus),
            '-' => Some(Punctuator::Minus),
            '*' => Some(Punctuator::Asterisk),
            '/' => Some(Punctuator::Slash),
            '%' => Some(Punctuator::Percent),
            '&' => Some(Punctuator::And),
            '|' => Some(Punctuator::Or),
            '^' => Some(Punctuator::Caret),
            '~' => Some(Punctuator::Tilde),
            '!' => Some(Punctuator::Bang),
            '<' => Some(Punctuator::LeftAngle),
            '>' => Some(Punctuator::RightAngle),
            '=' => Some(Punctuator::Equal),
            '?' => Some(Punctuator::Question),
            ':' => Some(Punctuator::Colon),
            ';' => Some(Punctuator::Semicolon),
            ',' => Some(Punctuator::Comma),
            '#' => Some(Punctuator::Hash),

            _ => None,
        }
    }
    pub fn stringify(&self) -> &'static str {
        match self {
            Punctuator::LBracket => "[",
            Punctuator::RBracket => "]",
            Punctuator::LParen => "(",
            Punctuator::RParen => ")",
            Punctuator::LBrace => "{",
            Punctuator::RBrace => "}",
            Punctuator::Dot => ".",
            Punctuator::Arrow => "->",
            Punctuator::PlusPlus => "++",
            Punctuator::MinusMinus => "--",
            Punctuator::And => "&",
            Punctuator::Asterisk => "*",
            Punctuator::Plus => "+",
            Punctuator::Minus => "-",
            Punctuator::Tilde => "~",
            Punctuator::Bang => "!",
            Punctuator::Slash => "/",
            Punctuator::Percent => "%",
            Punctuator::ShiftLeft => "<<",
            Punctuator::ShiftRight => ">>",
            Punctuator::LeftAngle => "<",
            Punctuator::RightAngle => ">",
            Punctuator::LessOrEqual => "<=",
            Punctuator::GreatorOrEqual => ">=",
            Punctuator::EqualEqual => "==",
            Punctuator::NotEqual => "!=",
            Punctuator::Caret => "^",
            Punctuator::Or => "|",
            Punctuator::AndAnd => "&&",
            Punctuator::OrOr => "||",
            Punctuator::Question => "?",
            Punctuator::Colon => ":",
            Punctuator::Semicolon => ";",
            Punctuator::DotDotDot => "...",
            Punctuator::Equal => "=",
            Punctuator::AsteriskEqual => "*=",
            Punctuator::SlashEqual => "/=",
            Punctuator::PercentEqual => "%=",
            Punctuator::PlusEqual => "+=",
            Punctuator::MinusEqual => "-=",
            Punctuator::ShiftLeftEqual => "<<=",
            Punctuator::ShiftRightEqual => ">>=",
            Punctuator::AndEqual => "&=",
            Punctuator::CaretEqual => "^=",
            Punctuator::OrEqual => "|=",
            Punctuator::Comma => ",",
            Punctuator::Hash => "#",
            Punctuator::HashHash => "##",
        }
    }
}

impl PreprocessingToken {
    pub(crate) fn as_header_name(&self) -> Option<(&String, &HeaderNameKind)> {
        if let Self::HeaderName(s, k) = self {
            Some((s, k))
        } else {
            None
        }
    }

    pub(crate) fn as_identifier(&self) -> Option<&String> {
        if let Self::Identifier(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
impl<'a> From<&'a str> for Lexer<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            source: value,
            location: Location { line: 1, col: 0, offset: 0 },
            directive_state: DirectiveState::Out,
        }
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HeaderNameKind {
    /// Angled brackets <>
    H,
    /// Normal quotations ""
    Q,
}
impl<'a> Lexer<'a> {
    pub fn tokenize(&'a mut self) -> Vec<Located<PreprocessingToken>> {
        let mut peekable = self.source.chars().peekmore();
        let mut tokens = vec![];
        while let Some(located_token) = self.next_token(&mut peekable) {
            self.location = located_token.end;
            if let Some(token) = located_token.value {
                tokens.push(Located {
                    value: token,
                    start: located_token.start,
                    end: located_token.end,
                });
            }
        }
        tokens
    }

    fn peek_next(&mut self, chars: &mut PeekMoreIterator<Chars>) -> Option<char> {
        // Copy the inner char out of the Option<char> so the mutable refernce created by `.peek()` dies faster
        let peeked = chars.peek().copied();
        match peeked {
            Some('\\') if *chars.peek_nth(1)? == '\n' => chars.peek_nth(2).copied(),
            Some(c) => Some(c),
            None => None,
        }
    }
    fn peek_nth(&mut self, chars: &mut PeekMoreIterator<Chars>, n: usize) -> Option<char> {
        // Copy the inner char out of the Option<char> so the mutable refernce created by `.peek()` dies faster
        let peeked = chars.peek_nth(n).copied();
        match peeked {
            Some('\\') if *chars.peek_nth(n + 1)? == '\n' => chars.peek_nth(n + 2).copied(),
            Some(c) => Some(c),
            None => None,
        }
    }
    // Wrap next, handling C's weird backline newline erasure feature
    fn next(&mut self, chars: &mut PeekMoreIterator<Chars>) -> Option<char> {
        match chars.next()? {
            '\\' if *chars.peek()? == '\n' => {
                chars.next()?; // consume '\n'
                self.location = self.location.newline();
                self.location = self.location.bump(1);
                chars.next()
            }
            c => Some(c),
        }
    }

    fn is_identifier_safe(start: bool, c: char) -> bool {
        match c {
            '_' | 'a'..='z' | 'A'..='Z' => true,
            '0'..='9' if !start => true,
            _ => false,
        }
    }

    fn is_header_name_char(c: char, kind: &HeaderNameKind) -> bool {
        match (kind, c) {
            (HeaderNameKind::H, '>' | '\n') => false,
            (HeaderNameKind::H, _) => true,
            (HeaderNameKind::Q, '"' | '\n') => false,
            (HeaderNameKind::Q, _) => true,
        }
    }

    /// ai slop warning, logic might not be right
    fn accumulate_while<F>(&mut self, chars: &mut PeekMoreIterator<Chars>, buf: &mut String, mut f: F) -> usize
    where
        F: FnMut(char) -> bool,
    {
        let mut iterations = 0;
        while let Some(next_c) = self.peek_next(chars) {
            if f(next_c) {
                buf.push(self.next(chars).unwrap());
            } else {
                break;
            }
            iterations += 1;
        }
        iterations
    }
    fn identifier(&mut self, chars: &mut PeekMoreIterator<Chars>, c: char) -> Option<(PreprocessingToken, usize)> {
        let mut identifier = String::from(c);

        let iterations = self.accumulate_while(chars, &mut identifier, |c| Self::is_identifier_safe(false, c));

        let token = PreprocessingToken::Identifier(identifier);

        Some((token, iterations))
    }
    /// returns None when theres an error, otherwise Always Some(Located(Tok)) where Tok is Some when a token was produced, None otherwise (whitespace for example)
    pub fn next_token<'b>(&'b mut self, chars: &mut PeekMoreIterator<Chars>) -> Option<Located<Option<PreprocessingToken>>> {
        let start = self.location;
        let located_preprocessing_token = match self.next(chars)? {
            c if Self::is_identifier_safe(true, c) => {
                let (token, iterations) = self.identifier(chars, c)?;

                if matches!(self.directive_state, DirectiveState::InDirective) && token.as_identifier().unwrap() == "include" {
                    self.directive_state = DirectiveState::InInclude;
                }
                start.located_until(self.location.bump(1 + iterations), Some(token))
            }
            '\n' => {
                self.directive_state = DirectiveState::Out;
                start.located_until(self.location.newline(), Some(PreprocessingToken::Newline))
            }
            c @ (' ' | '\t') => start.located_until(self.location.bump(1), Some(PreprocessingToken::Whitespace(c))),
            '/' if chars.peek() == Some(&'/') => {
                chars.next().unwrap(); // eat second /
                let mut comment = String::new();

                let comment_len = self.accumulate_while(chars, &mut comment, |c| c != '\n');

                start.located_until(self.location.bump(2 + comment_len), None)
            }
            '#' if self.peek_next(chars) == Some('#') => {
                self.next(chars)?;
                start.located_until(self.location.bump(2), Some(PreprocessingToken::Punctuator(Punctuator::HashHash)))
            }
            '#' if matches!(self.directive_state, DirectiveState::Out) => {
                self.directive_state = DirectiveState::InDirective;
                start.located_until(self.location.bump(1), Some(PreprocessingToken::Punctuator(Punctuator::Hash)))
            }
            c @ ('<' | '"') if matches!(self.directive_state, DirectiveState::InInclude) => {
                let kind = match c {
                    '<' => HeaderNameKind::H,
                    '"' => HeaderNameKind::Q,
                    _ => panic!("syntax error"),
                };

                self.directive_state = DirectiveState::InHeaderName;
                let mut header_name = String::new();
                let header_name_len = self.accumulate_while(chars, &mut header_name, |c| Self::is_header_name_char(c, &kind));
                chars.next().unwrap();
                // header_name.push('>');
                self.directive_state = DirectiveState::Out;
                start.located_until(self.location.bump(1 + header_name_len + 1), Some(PreprocessingToken::HeaderName(header_name, kind)))
            }
            '>' | '"' if matches!(self.directive_state, DirectiveState::InHeaderName) => {
                self.directive_state = DirectiveState::Out;

                start.located_until(self.location.bump(1), Some(PreprocessingToken::Punctuator(Punctuator::RightAngle)))
            }
            '"' => {
                let mut literal = String::new();
                let literal_len = self.accumulate_while(chars, &mut literal, |c| c != '"');
                self.next(chars)?; //eat closing `""

                start.located_until(self.location.bump(2 + literal_len), Some(PreprocessingToken::StringLiteral(literal)))
            }
            '.' if self.peek_nth(chars, 0) == Some('.') && self.peek_nth(chars, 1) == Some('.') => {
                self.next(chars)?;
                self.next(chars)?;

                start.located_until(self.location.bump(3), Some(PreprocessingToken::Punctuator(Punctuator::DotDotDot)))
            }
            c @ '0'..='9' | c @ '.' => {
                if c == '.' && !matches!(self.peek_next(chars), Some('0'..='9'),) {
                    start.located_until(self.location.bump(1), Some(PreprocessingToken::Punctuator(Punctuator::Dot)))
                } else {
                    let mut number = String::new();
                    number.push(c);
                    let mut exponent = false;
                    let number_len = self.accumulate_while(chars, &mut number, |c| {
                        if matches!(c, 'e' | 'E' | 'p' | 'P') {
                            exponent = true;
                        }
                        c == '.' || matches!(c, '0'..='9') || Self::is_identifier_safe(true, c) || (exponent && matches!(c, '+' | '-'))
                    });
                    start.located_until(self.location.bump(1 + number_len), Some(PreprocessingToken::Number(number)))
                }
            }
            '<' if self.peek_next(chars) == Some('=') => {
                self.next(chars)?;
                start.located_until(self.location.bump(2), Some(PreprocessingToken::Punctuator(Punctuator::LessOrEqual)))
            }

            '>' if self.peek_next(chars) == Some('=') => {
                self.next(chars)?;
                start.located_until(self.location.bump(2), Some(PreprocessingToken::Punctuator(Punctuator::GreatorOrEqual)))
            }
            '=' if self.peek_next(chars) == Some('=') => {
                self.next(chars)?;
                start.located_until(self.location.bump(2), Some(PreprocessingToken::Punctuator(Punctuator::EqualEqual)))
            }
            '+' if self.peek_next(chars) == Some('+') => {
                self.next(chars)?;
                start.located_until(self.location.bump(2), Some(PreprocessingToken::Punctuator(Punctuator::PlusPlus)))
            }
            '&' if self.peek_next(chars) == Some('&') => {
                self.next(chars)?;
                start.located_until(self.location.bump(2), Some(PreprocessingToken::Punctuator(Punctuator::AndAnd)))
            }
            '|' if self.peek_next(chars) == Some('|') => {
                self.next(chars)?;
                start.located_until(self.location.bump(2), Some(PreprocessingToken::Punctuator(Punctuator::OrOr)))
            }
            '/' if self.peek_next(chars) == Some('*') => {
                self.next(chars)?;
                let mut comment = 0;

                while let Some(c) = self.peek_next(chars) {
                    if c == '*' && chars.peek_nth(1) == Some(&'/') {
                        self.next(chars)?;
                        self.next(chars)?;
                        break;
                    }
                    self.next(chars)?;
                    comment += 1;
                }

                start.located_until(self.location.bump(2 + comment + 2), None)
            }
            c @ ('+' | '-' | '*' | '/' | '%') if self.peek_next(chars) == Some('=') => {
                self.next(chars)?;
                start.located_until(
                    self.location.bump(2),
                    Some(PreprocessingToken::Punctuator(match c {
                        '+' => Punctuator::PlusEqual,
                        '-' => Punctuator::MinusEqual,
                        '*' => Punctuator::AsteriskEqual,
                        '/' => Punctuator::SlashEqual,
                        '%' => Punctuator::PercentEqual,
                        _ => unimplemented!(),
                    })),
                )
            }

            c @ ('(' | ')' | '[' | ']' | '{' | '}' | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' | '!' | '<' | '>' | '=' | '?' | ':' | ';' | ',' | '#') => {
                start.located_until(self.location.bump(1), Some(PreprocessingToken::Punctuator(Punctuator::from_char(c).unwrap())))
            }
            x => start.located_until(self.location.bump(1), Some(PreprocessingToken::Error(x))),
        };

        Some(located_preprocessing_token)
    }
}

#[test]
pub fn test_pp_numbers() {
    let toks = |i: &str| Lexer::from(i).tokenize().iter().map(|loc| loc.value.clone()).collect::<Vec<_>>();
    assert_eq!(toks("123"), vec![PreprocessingToken::Number("123".to_string())]);
    assert_eq!(toks("123"), vec![PreprocessingToken::Number("123".to_string())]);
    assert_eq!(toks("3.14"), vec![PreprocessingToken::Number("3.14".to_string())]);
    assert_eq!(toks("0x1A3F"), vec![PreprocessingToken::Number("0x1A3F".to_string())]);
    assert_eq!(toks("1.2e-3"), vec![PreprocessingToken::Number("1.2e-3".to_string())]);
    assert_eq!(toks("5E+2"), vec![PreprocessingToken::Number("5E+2".to_string())]);
    assert_eq!(toks("0x1.2p4"), vec![PreprocessingToken::Number("0x1.2p4".to_string())]);
    assert_eq!(toks("1E"), vec![PreprocessingToken::Number("1E".to_string())]);
    assert_eq!(toks("3."), vec![PreprocessingToken::Number("3.".to_string())]);
    assert_eq!(toks("1Efoo"), vec![PreprocessingToken::Number("1Efoo".to_string())])
}
