use std::{collections::HashMap, iter::Peekable};

use cyntax_errors::{
    Diagnostic,
    errors::{UnmatchedDelimiter, UnterminatedTreeNode},
};
use cyntax_lexer::{
    Punctuator, SparseChars, Token, Whitespace, lexer::CharLocation, span, spanned::Spanned,
};
use expand::ExpandTokens;
use radix_trie::Trie;
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
    pub fn expand(&mut self) -> Vec<&'a Spanned<Token>> {
        let itt: Vec<TokenTree> = IntoTokenTree {
            source: self.file_source,
            tokens: self.tokens.iter().peekable(),
        }
        .collect();
        dbg!(&itt);

        let mut state = HashMap::new();
        let expanded: Vec<_> = ExpandTokens {
            source: self.file_source,
            state: &mut state,
            token_trees: itt.iter(),
        }
        .flatten().collect();

        dbg!(&expanded);
        expanded
    }
}
