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
    matcher::Matcher,
    prepend::PrependingPeekableIterator,
    tree::{ControlLine, TokenTree},
};
pub type ReplacementList<'src> = Vec<&'src Spanned<Token>>;
pub type PResult<T> = Result<T, Report>;

#[derive(Debug)]
pub struct Expander<'src, I: Debug + Iterator<Item = TokenTree<'src>>> {
    pub source: &'src str,
    pub token_trees: Matcher<'src, PrependingPeekableIterator<I>>,
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
    pub fn expand(&mut self) {
        while let Some(tt) = self.token_trees.next() {
            let t = self.expand_token_tree(tt);
            self.output.extend(t);
        }
    }
    pub fn expand_token_tree(&mut self, tt: TokenTree<'src>) -> Vec<Spanned<Token>> {
        let mut output = vec![];
        match tt {
            TokenTree::Delimited(opener, closer, body) => {
                let mut delimited_body = vec![];
                for token in body {
                    let token = self.expand_token_tree(token);
                    delimited_body.extend(token);
                }
                let range = opener.range.start..closer.range.end;
                output.push(Spanned::new(
                    range,
                    Token::Delimited {
                        opener: opener,
                        closer: closer,
                        inner_tokens: delimited_body,
                    },
                ));
            }
            TokenTree::Token(spanned @ span!(Token::Identifier(idenitfier))) => match self.macros.get(idenitfier) {
                Some(MacroDefinition::Function { parameter_list, replacment_list }) => {}
                Some(MacroDefinition::Object(replacement_list)) => {
                    self.token_trees.inner.prepend_extend(replacement_list.iter().map(|token| TokenTree::OwnedToken((*token).clone())));
                }
                _ => {
                    output.push(spanned.clone());
                }
            },
            TokenTree::OwnedToken(ref spanned @ span!(Token::Identifier(ref idenitfier))) => match self.macros.get(idenitfier) {
                Some(MacroDefinition::Function { parameter_list, replacment_list }) => {}
                Some(MacroDefinition::Object(replacement_list)) => {
                    self.token_trees.inner.prepend_extend(replacement_list.iter().map(|token| TokenTree::OwnedToken((*token).clone())));
                }
                _ => {
                    output.push(spanned.clone());
                }
            },
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
        output
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
    pub fn parse_parameters<'func>(&mut self, parameters: &'src Spanned<Token>) -> Vec<&'func String> {
        todo!()
    }
}
