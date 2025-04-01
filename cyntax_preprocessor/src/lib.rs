use std::{collections::HashMap, iter::Peekable};

use cyntax_errors::{
    Diagnostic,
    errors::{UnmatchedDelimiter, UnterminatedTreeNode},
};
use cyntax_lexer::{
    Punctuator, SparseChars, Token, Whitespace, lexer::CharLocation, span, spanned::Spanned,
};
use expand::ExpandTokens;
use tree::{IntoTokenTree, TokenTree};
mod expand;
mod tree;
pub struct Preprocessor<'a> {
    // macros and whatever
    file_source: &'a str,
    file_name: &'a str,
    tokens: &'a [Spanned<Token>],
}

impl<'a> Preprocessor<'a> {
    pub fn new(
        file_name: &'a str,
        file_source: &'a str,
        tokens: &'a [Spanned<Token>],
    ) -> Preprocessor<'a> {
        Self {
            file_source,
            file_name,
            tokens,
        }
    }
    pub fn create_token_tree(&mut self) {
        let itt: Vec<TokenTree> = IntoTokenTree {
            source: self.file_source,
            tokens: self.tokens.iter().peekable(),
        }
        .collect();
        let expanded = ExpandTokens { state: &mut HashMap::new(), token_trees: itt.iter() };
        dbg!(&itt);
    }
}
