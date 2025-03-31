use std::iter::Peekable;

use cyntax_lexer::{
    Punctuator, Token, Whitespace,
    lexer::{self, CharLocation, Lexer},
    span,
    spanned::Spanned,
};

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
        // dbg!(&itt);
    }
}
pub struct IntoTokenTree<'a> {
    source: &'a str,
    tokens: Peekable<core::slice::Iter<'a, Spanned<Token>>>,
}
impl<'a> Iterator for IntoTokenTree<'a> {
    type Item = TokenTree<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.tokens.next()?;
        match token {
            span!(Token::Punctuator(Punctuator::Directive)) => {
                // Skip empty directive, they're harmless
                if matches!(
                    self.tokens.peek(),
                    Some(span!(Token::Whitespace(Whitespace::Newline)))
                ) {
                    self.tokens.next().unwrap();

                    return self.next();
                }

                match self.tokens.peek() {
                    Some(span!(Token::Identifier(inner)))
                        if Self::is_equal_within_source(self.source, inner, "define") =>
                    {
                        todo!();
                    }
                    Some(span!(Token::Identifier(inner)))
                        if Self::is_equal_within_source(self.source, inner, "ifdef") =>
                    {
                        let _ifdef = self.tokens.next().unwrap();
                        let macro_identifier = self
                            .ignore_preceeding_whitespace(|this| this.tokens.next())
                            .expect("Expected macro_identifier after ifdef");
                        // todo: handle extra tokens
                        let body = self.until_closer();
                        dbg!(&macro_identifier);
                        dbg!(&body);
                    }
                    Some(span!(Token::Identifier(inner)))
                        if Self::is_equal_within_source(self.source, inner, "endif") =>
                    {
                        return Some(TokenTree::Endif)
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        Some(TokenTree::Token(token))
    }
}
impl<'a> IntoTokenTree<'a> {
    pub fn until_closer(&mut self) -> Vec<TokenTree<'a>>{
        let mut body = vec![];

        while let Some(token) = self.tokens.peek() {
            // if the next token is a directive
            if matches!(token, span!(Token::Punctuator(Punctuator::Directive))) {
                let directive = self.next().unwrap();
                if matches!(directive, TokenTree::Endif) {
                    break;
                } else {
                    body.push(directive)
                }
            } else {
                body.push(TokenTree::Token(self.tokens.next().unwrap()));
            }
        }
        body
    }
}
impl<'a> IntoTokenTree<'a> {
    pub fn is_equal_within_source(source: &'a str, left: &[CharLocation], right: &str) -> bool {
        let left = left
            .iter()
            .flat_map(|range| source[range.start..range.end].chars());
        let right = right.chars();

        left.eq(right)
    }
    pub fn ignore_preceeding_whitespace<T, F>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        while let Some(span!(Token::Whitespace(Whitespace::Space | Whitespace::Tab))) =
            self.tokens.peek()
        {
            self.tokens.next().unwrap();
        }
        f(self)
    }
}
#[derive(Debug)]
pub enum TokenTree<'a> {
    IfDef {
        condition: Spanned<Token>,
        body: Vec<TokenTree<'a>>,
        opposition: Option<Box<TokenTree<'a>>>
    },
    IfNDef {
        condition: Spanned<Token>,
        body: Vec<TokenTree<'a>>,
        opposition: Option<Box<TokenTree<'a>>>
    },
    If {
        condition: Vec<Spanned<Token>>,
        body: Vec<TokenTree<'a>>,
        opposition: Option<Box<TokenTree<'a>>>
    },
    ElseIf {
        condition: Vec<Spanned<Token>>,
        body: Vec<TokenTree<'a>>,
        opposition: Option<Box<TokenTree<'a>>>
    },
    Else {
        body: Vec<TokenTree<'a>>
    },
    Endif,
    Token(&'a Spanned<Token>),
}
