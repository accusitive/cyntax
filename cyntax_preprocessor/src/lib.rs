#![feature(iter_chain)]
#![feature(iter_intersperse)]
use cyntax_common::{ast::Token, spanned::Spanned};
use cyntax_errors::UnwrapDiagnostic;
use expand::Expander;
use prepend::PrependingPeekableIterator;
use tree::{IntoTokenTree, TokenTree};

mod expand;
mod macros;
mod prepend;
mod substitute;
mod tree;
mod tests;

pub struct Preprocessor<'src> {
    // macros and whatever
    file_source: &'src str,
    file_name: &'src str,
    token_trees: Vec<TokenTree>,
}

impl<'src> Preprocessor<'src> {
    pub fn new(file_name: &'src str, file_source: &'src str, tokens: &'src [Spanned<Token>]) -> Preprocessor<'src> {
        let itt = IntoTokenTree {
            source: file_source,
            tokens: tokens.iter().peekable(),
            expecting_opposition: false,
        }
        .collect::<Vec<_>>();

        Self { file_source, file_name, token_trees: itt }
    }
    pub fn expand(self) -> Vec<Spanned<Token>> {
        let mut expander = Expander::new(self.file_name, self.file_source, PrependingPeekableIterator::new(self.token_trees.into_iter()));
        expander.expand().unwrap_diagnostic("test.c", self.file_source);
        dbg!(&expander.expanding);
        expander.output
    }
}
