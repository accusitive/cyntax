use std::ops::Range;

pub mod lexer;
pub mod prelexer;
#[derive(Debug)]
pub struct StrPieces<'a> {
    pieces: Vec<&'a str>,
}
impl<'a> From<StrPieces<'a>> for String {
    fn from(value: StrPieces<'a>) -> Self {
        let mut s = String::new();
        for piece in value.pieces {
            s.push_str(piece);
        }
        s
    }
}
impl<'a> PartialEq for StrPieces<'a> {
    fn eq(&self, other: &Self) -> bool {
        let self_iter = self.pieces.iter().flat_map(|s| s.chars());
        let other_iter = other.pieces.iter().flat_map(|s| s.chars());
        self_iter.eq(other_iter) // Compare character by character
    }
}

#[derive(Debug)]
pub enum Token {
    Identifier(Vec<Range<usize>>),
    Whitespace(Whitespace),
    Punctuator(Punctuator),
}
#[derive(Debug)]
pub enum Whitespace {
    /// ` `
    Space,
    /// \n
    Newline,
    /// \t
    Tab,
}
#[derive(Debug)]
#[allow(dead_code)]
pub enum Punctuator {
    // Brackets and Parentheses
    LeftBracket,  // [
    RightBracket, // ]
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    Dot,          // .
    Arrow,        // ->

    // Unary and Increment/Decrement Operators
    Increment,   // ++
    Decrement,   // --
    Ampersand,   // &
    Asterisk,    // *
    Plus,        // +
    Minus,       // -
    Tilde,       // ~
    Exclamation, // !

    // Arithmetic and Bitwise Operators
    Slash,        // /
    Percent,      // %
    ShiftLeft,    // <<
    ShiftRight,   // >>
    LessThan,     // <
    GreaterThan,  // >
    LessEqual,    // <=
    GreaterEqual, // >=
    Equal,        // ==
    NotEqual,     // !=
    Caret,        // ^
    Pipe,         // |
    And,          // &&
    Or,           // ||

    // Ternary and Colon Operators
    Question,  // ?
    Colon,     // :
    Semicolon, // ;
    Ellipsis,  // ...

    // Assignment Operators
    Assign,    // =
    MulAssign, // *=
    DivAssign, // /=
    ModAssign, // %=
    AddAssign, // +=
    SubAssign, // -=
    ShlAssign, // <<=
    ShrAssign, // >>=
    AndAssign, // &=
    XorAssign, // ^=
    OrAssign,  // |=

    // Miscellaneous
    Comma,    // ,
    Hash,     // #
    HashHash, // ##

    // Digraphs (Alternative Tokens)
    LessColon,                // <:
    ColonGreater,             // :>
    LessPercent,              // <%
    PercentGreater,           // %>
    PercentColon,             // %:
    PercentColonPercentColon, // %:%:
}

impl Punctuator {
    pub fn to_string(&self) -> String {
        match self {
            Punctuator::LeftBracket => "[".to_string(),
            Punctuator::RightBracket => "".to_string(),
            Punctuator::LeftParen => "(".to_string(),
            Punctuator::RightParen => ")".to_string(),
            Punctuator::LeftBrace => "{".to_string(),
            Punctuator::RightBrace => "}".to_string(),
            Punctuator::Dot => ".".to_string(),
            Punctuator::Arrow => "->".to_string(),
            Punctuator::Increment => "++".to_string(),
            Punctuator::Decrement => "--".to_string(),
            Punctuator::Ampersand => "&".to_string(),
            Punctuator::Asterisk => "*".to_string(),
            Punctuator::Plus => "+".to_string(),
            Punctuator::Minus => "-".to_string(),
            Punctuator::Tilde => "~".to_string(),
            Punctuator::Exclamation => "!".to_string(),
            Punctuator::Slash => "/".to_string(),
            Punctuator::Percent => "%".to_string(),
            Punctuator::ShiftLeft => "<<".to_string(),
            Punctuator::ShiftRight => ">>".to_string(),
            Punctuator::LessThan => "<".to_string(),
            Punctuator::GreaterThan => ">".to_string(),
            Punctuator::LessEqual => "<=".to_string(),
            Punctuator::GreaterEqual => "".to_string(),
            Punctuator::Equal => "==".to_string(),
            Punctuator::NotEqual => "!=".to_string(),
            Punctuator::Caret => "^".to_string(),
            Punctuator::Pipe => "|".to_string(),
            Punctuator::And => "&&".to_string(),
            Punctuator::Or => "||".to_string(),
            Punctuator::Question => "?".to_string(),
            Punctuator::Colon => ":".to_string(),
            Punctuator::Semicolon => ";".to_string(),
            Punctuator::Ellipsis => "...".to_string(),
            Punctuator::Assign => "=".to_string(),
            Punctuator::MulAssign => "*=".to_string(),
            Punctuator::DivAssign => "/=".to_string(),
            Punctuator::ModAssign => "%=".to_string(),
            Punctuator::AddAssign => "+=".to_string(),
            Punctuator::SubAssign => "-=".to_string(),
            Punctuator::ShlAssign => "<<=".to_string(),
            Punctuator::ShrAssign => ">>=".to_string(),
            Punctuator::AndAssign => "&=".to_string(),
            Punctuator::XorAssign => "^=".to_string(),
            Punctuator::OrAssign => "|=".to_string(),
            Punctuator::Comma => ",".to_string(),
            Punctuator::Hash => "#".to_string(),
            Punctuator::HashHash => "##".to_string(),
            Punctuator::LessColon => "<:".to_string(),
            Punctuator::ColonGreater => "".to_string(),
            Punctuator::LessPercent => "<%".to_string(),
            Punctuator::PercentGreater => "%>".to_string(),
            Punctuator::PercentColon => "%:".to_string(),
            Punctuator::PercentColonPercentColon => "%:%:".to_string(),
        }
    }
}
