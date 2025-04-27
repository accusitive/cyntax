#![feature(iter_chain)]
#![feature(iter_intersperse)]
use cyntax_common::{
    ast::PreprocessingToken,
    ctx::{ParseContext, HasContext},
    spanned::Spanned,
};
use expand::{Expander, PResult};
use prepend::PrependingPeekableIterator;
use tree::{IntoTokenTree, TokenTree};

mod expand;
mod macros;
mod prepend;
mod substitute;
mod tree;

pub struct Preprocessor<'src> {
    // macros and whatever
    ctx: &'src mut ParseContext,
    token_trees: Vec<TokenTree>,
}
impl<'src> HasContext for Preprocessor<'src> {
    fn ctx(&self) -> &ParseContext {
        self.ctx
    }
}
impl<'src> Preprocessor<'src> {
    pub fn new(ctx: &'src mut ParseContext, tokens: &'src [Spanned<PreprocessingToken>]) -> Preprocessor<'src> {
        let itt = IntoTokenTree {
            ctx,
            tokens: tokens.iter().peekable(),
            expecting_opposition: false,
        }
        .collect::<Vec<_>>();

        Self { ctx, token_trees: itt }
    }
    pub fn expand(self) -> PResult<Vec<Spanned<PreprocessingToken>>> {
        let mut expander = Expander::new(self.ctx, PrependingPeekableIterator::new(self.token_trees.into_iter()));
        expander.expand()?;
        dbg!(&expander.expanding);
        Ok(expander.output)
    }
}
