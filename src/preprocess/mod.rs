use std::slice::Iter;

use codespan_reporting::diagnostic::Diagnostic;
use peekmore::PeekMoreIterator;

use crate::{lexer::PreprocessingToken, location::LocationHistory};

macro_rules! loc {
    ($p: pat) => {
        L { value: $p, .. }
    };
}

type PPResult<T> = Result<T, Diagnostic<usize>>;
type TokenStream<'a> = PeekMoreIterator<Iter<'a, LocationHistory<PreprocessingToken>>>;
type L<T> = LocationHistory<T>;

pub mod ast;
pub mod tree;

#[derive(Debug)]
pub struct Preprocessor {}

impl Preprocessor {
    pub fn new() -> Self {
        Self {}
    }
    pub fn next_non_whitespace_token<'a: 'b, 'b>(&mut self, token_stream: &'a mut TokenStream) -> PPResult<LocationHistory<PreprocessingToken>> {
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
}
