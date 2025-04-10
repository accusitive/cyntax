#![feature(iter_chain)]

use std::collections::HashMap;

use cyntax_common::{ast::Token, spanned::Spanned};
use expand::Expander;
use prepend::PrependingPeekableIterator;
use tree::{IntoTokenTree, TokenTree};

mod expand;
mod macros;
mod prepend;
mod tree;
mod substitute;
pub struct Preprocessor<'src> {
    // macros and whatever
    file_source: &'src str,
    file_name: &'src str,
    token_trees: Vec<TokenTree<'src>>,
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
        // let mut state = HashMap::new();
        let tt = self.token_trees;

        let mut expander = Expander::new(self.file_source, PrependingPeekableIterator::new(tt.into_iter()));
        expander.expand();

        expander.output
    }
}

mod tests {
    use cyntax_common::ast::{Token, Whitespace};
    use cyntax_lexer::lexer::Lexer;

    use crate::Preprocessor;
    fn test_helper(source: &str) -> Vec<Token> {
        let tokens = Lexer::new("test.c", source).collect::<Vec<_>>();
        let pp = Preprocessor::new("test.c", source, &tokens).expand();
        let despanned = pp.into_iter().map(|token| token.value).filter(|token| !matches!(token, Token::Whitespace(_))).collect::<Vec<_>>();

        despanned
    }
    #[test]
    fn test_simple() {
        let source = r#"
#define a 5
a"#;
        assert_eq!(test_helper(source), vec![Token::PPNumber("5".to_string())]);
    }
    #[test]
    fn test_recursive_object_macros() {
        let source = r#"
#define a b
#define b a
a"#;
        assert_eq!(test_helper(source), vec![Token::BlueIdentifier("a".to_string())]);
    }
}
