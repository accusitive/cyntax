use std::ops::Range;

use strum_macros::EnumString;

use crate::{spanned::Spanned, sparsechars::SparseChars};


#[derive(Debug)]
pub enum Token {
    Identifier(SparseChars),
    StringLiteral(Vec<Range<usize>>),
    PPNumber(SparseChars),

    // Directive(Directive),
    ControlLine(Vec<Spanned<Token>>),

    Whitespace(Whitespace),
    Punctuator(Punctuator),

    Delimited(char, char, Spanned<Vec<Spanned<Token>>>),
}
#[derive(Debug)]
pub enum Directive {
    DefineObject(Spanned<SparseChars>, Vec<Spanned<Token>>),
    DefineFunction(
        Spanned<SparseChars>,
        Spanned<Vec<Spanned<Token>>>,
        Vec<Spanned<Token>>,
    ),
    Undefine(Spanned<SparseChars>),
}
#[derive(Debug, PartialEq)]
pub enum Whitespace {
    /// ` `
    Space,
    /// \n
    Newline,
    /// \t
    Tab,
}


#[derive(Debug, PartialEq)]
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
    Comma,     // ,
    Directive, // #, but only if its the first non-whitespace character of the line
    Hash,      // #
    HashHash,  // ##

    // Digraphs (Alternative Tokens)
    LessColon,                // <:
    ColonGreater,             // :>
    LessPercent,              // <%
    PercentGreater,           // %>
    PercentColon,             // %:
    PercentColonPercentColon, // %:%:
}

#[derive(Debug, Clone, PartialEq, Eq, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    #[strum(serialize = "_Bool")]
    Bool,
    #[strum(serialize = "_Complex")]
    Complex,
    #[strum(serialize = "_Imaginary")]
    Imaginary,
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
            // Use alternate character `♯` instead of `#` for debug printing, to make it clear that its a Directive not a Hash
            Punctuator::Directive => "♯".to_string(),
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
    pub fn is_punctuation(c: char) -> bool {
        Punctuator::from_char(c).is_some()
    }
    pub fn from_char(c: char) -> Option<Punctuator> {
        match c {
            '[' => Some(Punctuator::LeftBracket),
            ']' => Some(Punctuator::RightBracket),
            '(' => Some(Punctuator::LeftParen),
            ')' => Some(Punctuator::RightParen),
            '{' => Some(Punctuator::LeftBrace),
            '}' => Some(Punctuator::RightBrace),
            '.' => Some(Punctuator::Dot),
            '&' => Some(Punctuator::Ampersand),
            '*' => Some(Punctuator::Asterisk),
            '+' => Some(Punctuator::Plus),
            '-' => Some(Punctuator::Minus),
            '~' => Some(Punctuator::Tilde),
            '!' => Some(Punctuator::Exclamation),
            '/' => Some(Punctuator::Slash),
            '%' => Some(Punctuator::Percent),
            '<' => Some(Punctuator::LessThan),
            '>' => Some(Punctuator::GreaterThan),
            '^' => Some(Punctuator::Caret),
            '|' => Some(Punctuator::Pipe),
            '?' => Some(Punctuator::Question),
            ':' => Some(Punctuator::Colon),
            ';' => Some(Punctuator::Semicolon),
            '=' => Some(Punctuator::Assign),
            ',' => Some(Punctuator::Comma),
            '#' => Some(Punctuator::Hash),
            _ => None,
        }
    }
}
