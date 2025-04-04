#![feature(iter_chain)]

use std::collections::HashMap;

use cyntax_common::{ast::Token, spanned::Spanned};
use expand::Expander;
use prepend::PrependingPeekableIterator;
use tree::{IntoTokenTree, TokenTree};

mod tree;
mod prepend;
mod expand;
pub struct Preprocessor<'src> {
    // macros and whatever
    file_source: &'src str,
    file_name: &'src str,
    token_trees: Vec<TokenTree<'src>>,
}

impl<'src> Preprocessor<'src> {
    pub fn new(
        file_name: &'src str,
        file_source: &'src str,
        tokens: &'src [Spanned<Token>],
    ) -> Preprocessor<'src> {
        let itt = IntoTokenTree {
            source: file_source,
            tokens: tokens.iter().peekable(),
            expecting_opposition: false,
        }
        .collect::<Vec<_>>();

        Self {
            file_source,
            file_name,
            token_trees: itt,
        }
    }
    pub fn expand(self) -> Vec<Spanned<Token>> {
        // let mut state = HashMap::new();
        let tt = self.token_trees;

        let mut expander = Expander {
            source: self.file_source,
            token_trees: PrependingPeekableIterator::new(tt.into_iter()),
            output: vec![],
            macros: HashMap::new(),
            eof: false
        };
        expander.expand();

        expander.output
    }
}
