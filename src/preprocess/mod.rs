use std::{collections::HashMap, slice::Iter};

use ast::{Constant, FloatConstant, IntConstant, Token};
use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles};
use peekmore::PeekMoreIterator;

use crate::{lexer::PreprocessingToken, location::LocationHistory, parser::ast::Keyword};

macro_rules! loc {
    ($p: pat) => {
        L { value: $p, .. }
    };
}

type PPResult<T> = Result<T, Diagnostic<usize>>;
type TokenStream<'a> = PeekMoreIterator<Iter<'a, LocationHistory<PreprocessingToken>>>;
type L<T> = LocationHistory<T>;

pub mod ast;
pub mod eval;
pub mod expand;
pub mod tree;
pub mod include;
#[derive(Debug)]
pub struct Preprocessor {
    pub macros: HashMap<String, Macro>,
    pub files: SimpleFiles<String, String,>
}
#[derive(Debug)]
pub enum Macro {
    Object(Vec<L<PreprocessingToken>>),
    Function(Vec<String>, Vec<L<PreprocessingToken>>)
}

impl Macro {
    pub fn as_object(&self) -> Option<&Vec<L<PreprocessingToken>>> {
        if let Self::Object(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn as_function(&self) -> Option<(&Vec<String>, &Vec<L<PreprocessingToken>>)> {
        if let Self::Function(params, replacement_list) = self {
            Some((params, replacement_list))
        } else {
            None
        }
    }
}

impl Preprocessor {
    pub fn new() -> Self {
        Self { macros: HashMap::new(), files: SimpleFiles::new() }
    }
    pub fn next_non_whitespace_token(&self, token_stream: &mut TokenStream) -> PPResult<LocationHistory<PreprocessingToken>> {
        match token_stream.next() {
            Some(loc!(PreprocessingToken::Whitespace(_))) => self.next_non_whitespace_token(token_stream),
            Some(t) => Ok(t.clone()),
            None => Err(Diagnostic::bug().with_message("Unexpected EOF")),
        }
    }
    pub fn collect_until_newline(&mut self, token_stream: &mut TokenStream) -> Vec<LocationHistory<PreprocessingToken>> {
        let mut tokens = vec![];
        while let Some(t) = token_stream.peek() {
            if matches!(t.value, PreprocessingToken::Newline) {
                break;
            } else {
                tokens.push(token_stream.next().unwrap().clone());
            }
        }

        tokens
    }
    pub fn pp_token_to_token(token: LocationHistory<PreprocessingToken>) -> Option<LocationHistory<Token>> {
        let token_shell = token.shell();
        if let loc!(PreprocessingToken::Whitespace(_)) = token {
            return None;
        }
        Some(token_shell.same(match token.value {
            PreprocessingToken::Identifier(i) if Keyword::from_str(&i).is_some() => Token::Keyword(Keyword::from_str(&i).unwrap()),
            PreprocessingToken::Identifier(i) => Token::Identifier(i),
            PreprocessingToken::Number(n) => {
                let constant = if n.contains(".") {
                    Constant::Float(FloatConstant::parse(&token_shell.same(n.clone())).unwrap())
                } else {
                    Constant::Integer(IntConstant::parse(&token_shell.same(n.clone())).unwrap())
                };
                Token::Constant(constant)
            }
            PreprocessingToken::StringLiteral(s) => Token::StringLiteral(s),
            PreprocessingToken::Punctuator(p) => Token::Punctuator(p),

            token => unimplemented!("processing for token {:?} not implemented", token),
        }))
    }
}
