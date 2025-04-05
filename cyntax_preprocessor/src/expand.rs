use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
use cyntax_errors::Diagnostic;
use cyntax_errors::{UnwrapDiagnostic, why::Report};
use cyntax_lexer::span;
use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    iter::Peekable,
};

use crate::{
    prepend::PrependingPeekableIterator,
    tree::{ControlLine, TokenTree},
};
pub type ReplacementList<'src> = Vec<&'src Spanned<Token>>;
pub type PResult<T> = Result<T, Report>;

#[derive(Debug)]
pub struct Expander<'src, I: Debug + Iterator<Item = TokenTree<'src>>> {
    pub source: &'src str,
    pub token_trees: PrependingPeekableIterator<I>,
    pub output: Vec<Spanned<Token>>,
    pub macros: HashMap<&'src String, MacroDefinition<'src>>,
    pub eof: bool,
}
#[derive(Debug, Clone)]
pub enum MacroDefinition<'src> {
    Object(ReplacementList<'src>),
    Function { parameter_list: Vec<&'src String>, replacment_list: ReplacementList<'src> },
}

impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn expand(&mut self) -> PResult<()> {
        while !self.eof {
            let expanded = self.expand_one().unwrap();
            self.output.extend(expanded);
        }
        Ok(())
    }
    pub fn expand_one(&mut self) -> PResult<Vec<Spanned<Token>>> {
        let mut output = vec![];
        if let Some(token_tree) = self.token_trees.next() {
            dbg!(&token_tree);
            match token_tree {
                //Its ugly but we produce OwnedTokens when we reinject tokens back into the token stream
                TokenTree::OwnedToken(span!(Token::Identifier(identifier))) if self.macros.get(&identifier).is_some() => {
                    // let tt = self.expect_tt_token2(&token_tree).unwrap();
                    self.expand_identifier(&identifier)?;
                }

                TokenTree::Token(tok @ span!(Token::Identifier(identifier))) if self.macros.get(identifier).is_some() => {
                    self.expand_identifier(identifier)?;
                }

                TokenTree::OwnedToken(span!(Token::Delimited { opener, closer: None, inner_tokens })) => {
                    let mut body = vec![];
                    while let Some(token) = self.get_expanded_next_token() {
                        if matches!(token, span!(Token::Punctuator(Punctuator::RightParen))) {
                            break;
                        } else {
                            body.push(token);
                        }
                    }
                    output.push(Spanned::new(
                        0..0,
                        Token::Delimited {
                            opener: opener,
                            closer: Some(')'),
                            inner_tokens: Spanned::new(0..0, vec![inner_tokens.value, body].into_iter().flatten().collect()),
                        },
                    ));
                }

                TokenTree::Token(spanned) => {
                    output.push(spanned.clone());
                }
                TokenTree::OwnedToken(spanned) => {
                    output.push(spanned);
                }
                TokenTree::Directive(control_line) => {
                    self.handle_control_line(control_line);
                }

                _ => {}
            }
        } else {
            self.eof = true;
        }
        Ok(output)
    }
    // this is how i handle macros creating partial delimiters
    // maybe this is a bad solution?
    pub fn get_expanded_next_token(&mut self) -> Option<Spanned<Token>> {
        dbg!(&self.token_trees.peek());
        let mut output = self.expand_one().unwrap();
        assert_eq!(output.len(), 1);
        let output = output.pop();
        dbg!(&output);
        output
    }

    pub fn expand_identifier(&mut self, identifier: &String) -> PResult<()> {
        match { self.macros.get(identifier).unwrap().clone() } {
            MacroDefinition::Object(replacement_list) => {
                self.token_trees.prepend_extend(replacement_list.iter().map(|token| TokenTree::Token(*token)));
            }
            MacroDefinition::Function { parameter_list: parameters, replacment_list } => match self.expand_function_style_macro(&parameters, &replacment_list) {
                Some(_) => {}
                None => {
                    self.output.push(Spanned::new(0..0, Token::Identifier(identifier.clone())));
                }
            },
        }
        Ok(())
    }
    pub fn expand_function_style_macro(&mut self, parameters: &Vec<&'src String>, replacment_list: &Vec<&'src Spanned<Token>>) -> Option<()> {
        if self.token_trees.peek().is_none() {
            return None;
        }
        let argument_list = self.get_expanded_next_token();
        if argument_list.is_none() {
            return None;
        }
        let argument_list = argument_list.unwrap();
        dbg!(&argument_list);
        let argument_container = self.expect_delimited(&argument_list);
        if argument_container.is_err() {
            return None;
        }
        let argument_container = argument_container.unwrap();
        let split_delimited = self.split_delimited(&argument_container.2);
        let mut map: HashMap<String, Vec<Spanned<Token>>> = HashMap::new();
        for (&param, arg) in parameters.iter().zip(split_delimited.iter()) {
            map.insert(param.to_string(), Self::i_hate_this(arg));
        }

        let replacement_list = self
            .apply(
                &|token| match &token {
                    span!(Token::Identifier(i)) => {
                        if let Some(e) = map.get(i) {
                            e.to_vec()
                        } else {
                            vec![token]
                        }
                    }
                    _ => vec![token],
                },
                Self::i_hate_this(replacment_list).into_iter(),
            )
            .into_iter()
            .map(|token| TokenTree::OwnedToken(token));

        self.token_trees.prepend_extend(replacement_list);
        Some(())
    }

    pub fn handle_control_line(&mut self, control_line: ControlLine<'src>) {
        match control_line {
            ControlLine::DefineFunction { macro_name, parameters, replacement_list } => self.handle_define_function(macro_name, parameters, &replacement_list),
            ControlLine::DefineObject { macro_name, replacement_list } => self.handle_define_object(macro_name, &replacement_list),
            _ => todo!(),
        }
    }
    pub fn handle_define_function<'func>(&mut self, macro_name: &'src String, parameters: &'src Spanned<Token>, replacment_list: &'func Vec<&'src Spanned<Token>>) {
        let parameters = self.parse_parameters(parameters);
        self.macros.insert(
            macro_name,
            MacroDefinition::Function {
                parameter_list: parameters.clone(),
                replacment_list: replacment_list.to_vec(),
            },
        );
    }
    pub fn handle_define_object<'func>(&mut self, macro_name: &'src String, replacment_list: &'func Vec<&'src Spanned<Token>>) {
        self.macros.insert(macro_name, MacroDefinition::Object(replacment_list.to_vec()));
    }
    pub fn i_hate_this<T: Clone>(vec: &Vec<&T>) -> Vec<T> {
        vec.to_vec().iter().map(|tok| (*tok).clone()).collect()
    }
    pub fn enforce_identifier(&self, token: &'src Spanned<Token>) -> &'src String {
        match token {
            span!(Token::Identifier(inner)) => return inner,
            _ => {
                // TODO: Error
                panic!("expected and identifier, found {:#?}", token);
            }
        }
    }
    pub fn expect_comma(&self, token: &'src Spanned<Token>) -> Option<()> {
        match token {
            span!(Token::Punctuator(Punctuator::Comma)) => Some(()),
            _ => None,
        }
    }
    pub fn expect_tt_token<'a>(&self, token_tree: TokenTree<'src>) -> Option<Spanned<Token>> {
        match token_tree {
            TokenTree::Token(spanned) => Some(spanned.clone()),
            TokenTree::OwnedToken(spanned) => Some(spanned),
            _ => None,
        }
    }
    pub fn expect_tt_token2<'a>(&self, token_tree: &TokenTree<'src>) -> Option<Spanned<Token>> {
        match token_tree {
            TokenTree::Token(spanned) => Some((*spanned).clone()),
            TokenTree::OwnedToken(spanned) => Some(spanned.clone()),
            _ => None,
        }
    }
    pub fn parse_parameters(&self, delimited: &'src Spanned<Token>) -> Vec<&'src String> {
        match delimited {
            span!(Token::Delimited { opener: '(', closer: Some(')'), inner_tokens }) => {
                let mut inner_iter = inner_tokens.value.iter().peekable();
                let mut parameters = vec![];
                while let Some(token) = inner_iter.peek() {
                    dbg!(&parameters, &token);

                    if parameters.len() > 0 {
                        self.expect_comma(token).expect("expected comma");
                        inner_iter.next().unwrap();
                    }
                    if inner_iter.len() == 0 {
                        // TODO: error about trailing comma
                        break;
                    }

                    let identifier = self.enforce_identifier(inner_iter.next().unwrap());

                    parameters.push(identifier);
                }
                parameters
            }
            _ => unreachable!(),
        }
    }

    // Returns an array, with one element for each token stretch surrounding commas.
    // [,,] -> [[], []]
    // [2+5] -> [[2+5]]
    // [2+5,] -> [[2+5], []]
    pub fn split_delimited<'a>(&self, tokens: &'a Spanned<Vec<Spanned<Token>>>) -> Vec<Vec<&'a Spanned<Token>>> {
        let mut element = vec![];
        let mut elements = vec![];

        for token in &tokens.value {
            if matches!(token, span!(Token::Punctuator(Punctuator::Comma))) {
                elements.push(std::mem::replace(&mut element, Vec::new()));
            } else {
                element.push(token);
            }
        }
        elements.push(std::mem::replace(&mut element, Vec::new()));

        dbg!(&elements);
        elements
    }
    pub fn expect_delimited<'a>(&'a self, token: &'a Spanned<Token>) -> PResult<(&'a char, &'a char, &'a Spanned<Vec<Spanned<Token>>>)> {
        match token {
            span!(Token::Delimited { opener: open, closer: Some(close), inner_tokens }) => Ok((open, close, inner_tokens)),
            span!(range, token2) => Err(cyntax_errors::errors::ExpectedButFound {
                location: range.start..range.end,
                expected: format!("expected: Delimiter with closer"),
                found: format!("found: {:?}", token2),
            }
            .into_why_report()),
        }
    }
    pub fn next_non_whitespace(&mut self) -> Option<TokenTree<'src>> {
        match self.token_trees.next()? {
            TokenTree::OwnedToken(span!(Token::Whitespace(_))) | TokenTree::Token(span!(Token::Whitespace(_))) => self.next_non_whitespace(),

            token => {
                return Some(token);
            }
        }
    }
    pub fn apply<TokenStream: Iterator<Item = Spanned<Token>>, F: Fn(Spanned<Token>) -> Vec<Spanned<Token>>>(&mut self, f: &F, tokens: TokenStream) -> Vec<Spanned<Token>> {
        tokens
            .into_iter()
            .map(|token| match token {
                span!(Token::Delimited { opener, closer, inner_tokens }) => {
                    vec![Spanned::new(
                        token.range,
                        Token::Delimited {
                            opener: opener,
                            closer: closer,
                            inner_tokens: Spanned::new(inner_tokens.range, self.apply(f, inner_tokens.value.into_iter())),
                        },
                    )]
                }
                _ => f(token),
            })
            .flatten()
            .collect()
    }
}
