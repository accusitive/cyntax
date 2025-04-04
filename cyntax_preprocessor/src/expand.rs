use std::collections::{HashMap, VecDeque};

use cyntax_common::{
    ast::Token,
    spanned::Spanned,
    sparsechars::{HashedSparseChars, SparseChars},
};
use cyntax_lexer::span;

use crate::tree::{ControlLine, TokenTree};
pub type ReplacementList<'src> = Vec<&'src Spanned<Token>>;

pub struct PrependingIterator<I: Iterator> {
    queue: VecDeque<I::Item>,
    inner: I
}
pub struct Expander<'src, I: Iterator<Item = TokenTree<'src>>> {
    pub source: &'src str,
    pub token_trees: I,
    pub rescan_queue: Vec<TokenTree<'src>>,
    pub output: Vec<Spanned<Token>>,
    pub macros: HashMap<HashedSparseChars, MacroDefinition<'src>>,
}
pub enum MacroDefinition<'src> {
    Object(ReplacementList<'src>),
    Function {
        parameters: Spanned<Token>,
        replacment_list: Vec<Spanned<Token>>,
    },
}

impl<'src, I: Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn expand(&mut self) {
        while let Some(token) = self.next_token() {
            match token {
                TokenTree::Token(span!(Token::Identifier(identifier)))
                    if self.macros.get(&identifier.hash(self.source)).is_some() =>
                {
                    match self.macros.get(&identifier.hash(self.source)).unwrap() {
                        MacroDefinition::Object(spanneds) => {
                            self.rescan_queue
                                .extend(spanneds.iter().map(|token| TokenTree::Token(token)));
                        }
                        MacroDefinition::Function {
                            parameters,
                            replacment_list,
                        } => todo!(),
                    }
                }
                TokenTree::Token(spanned) => {
                    self.output.push(spanned.clone());
                }
                TokenTree::Directive(control_line) => {
                    self.handle_control_line(control_line);
                }

                _ => {}
            }
        }
    }
    pub fn handle_control_line(&mut self, control_line: ControlLine<'src>) {
        match control_line {
            ControlLine::DefineFunction {
                macro_name,
                parameters,
                replacement_list,
            } => self.handle_define_function(macro_name, parameters, &replacement_list),
            ControlLine::DefineObject {
                macro_name,
                replacement_list,
            } => self.handle_define_object(macro_name, &replacement_list),
            _ => todo!(),
        }
    }
    pub fn handle_define_function(
        &mut self,
        macro_name: &SparseChars,
        parameters: &Spanned<Token>,
        replacment_list: &Vec<&Spanned<Token>>,
    ) {
        self.macros.insert(
            macro_name.hash(self.source),
            MacroDefinition::Function {
                parameters: parameters.clone(),
                replacment_list: Self::i_hate_this(replacment_list),
            },
        );
    }
    pub fn handle_define_object<'a>(
        &mut self,
        macro_name: &SparseChars,
        replacment_list: &'a Vec<&'src Spanned<Token>>,
    ) {
        self.macros.insert(
            macro_name.hash(self.source),
            MacroDefinition::Object(replacment_list.to_vec()),
        );
    }
    pub fn i_hate_this<T: Clone>(vec: &Vec<&T>) -> Vec<T> {
        vec.to_vec().iter().map(|tok| (*tok).clone()).collect()
    }
    pub fn next_token(&mut self) -> Option<TokenTree<'src>> {
        if let Some(tt) = self.rescan_queue.pop() {
            Some(tt)
        } else {
            self.token_trees.next()
        }
    }
}
