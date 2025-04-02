use std::collections::HashMap;

use cyntax_common::{ast::Token, spanned::Spanned};
use expand::ExpandTokens;
use tree::{IntoTokenTree, TokenTree};
mod expand;
mod tree;
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
            expecting_opposition: false
        }
        .collect::<Vec<_>>();

        Self {
            file_source,
            file_name,
            token_trees: itt,
        }
    }
    pub fn expand(&mut self) -> Vec<&Spanned<Token>> {
        let mut state = HashMap::new();
        let tt = &mut self.token_trees;
        let expanded = ExpandTokens {
            source: self.file_source,
            state: &mut state,
            token_trees: tt.iter(),
        }
        .flatten()
        .collect::<Vec<_>>();

        expanded
    }
}
