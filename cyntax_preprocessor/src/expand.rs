use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    iter::Peekable,
};

use cyntax_common::{ast::Token, spanned::Spanned};
use cyntax_lexer::span;

use crate::tree::{ControlLine, TokenTree};
pub type ReplacementList<'src> = Vec<&'src Spanned<Token>>;

#[derive(Debug)]
pub struct PrependingPeekableIterator<I: Iterator + Debug> {
    pub queue: VecDeque<I::Item>,
    inner: Peekable<I>,
}
impl<I: Iterator + Debug> Iterator for PrependingPeekableIterator<I>
where
    I::Item: Debug,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.queue.pop_front() {
            return Some(item);
        } else {
            self.inner.next()
        }
    }
}
impl<I: Iterator + Debug> PrependingPeekableIterator<I> {
    pub fn new(i: I) -> Self {
        Self {
            queue: VecDeque::new(),
            inner: i.peekable(),
        }
    }
    // pub fn peek(&mut self) -> Option<&I::Item> {
    //     if let Some(front) = self.queue.back() {
    //         return Some(front);
    //     } else {
    //         self.inner.peek()
    //     }
    // }
    pub fn prepend_extend<J: Iterator<Item = I::Item>>(&mut self, mut iter: J)
    where
        J::Item: Debug,
    {
        let mut index = 0;
        while let Some(item) = iter.next() {
            self.queue.insert(index, item);
            index += 1;
        }
    }
}
#[derive(Debug)]
pub struct Expander<'src, I: Debug + Iterator<Item = TokenTree<'src>>> {
    pub source: &'src str,
    pub token_trees: PrependingPeekableIterator<I>,
    pub output: Vec<Spanned<Token>>,
    pub macros: HashMap<&'src String, MacroDefinition<'src>>,
}
#[derive(Debug)]
pub enum MacroDefinition<'src> {
    Object(ReplacementList<'src>),
    Function {
        parameters: Spanned<Token>,
        replacment_list: Vec<Spanned<Token>>,
    },
}

impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn expand(&mut self) {
        while let Some(token) = self.token_trees.next() {
            match token {
                TokenTree::Token(span!(Token::Identifier(identifier)))
                    if self.macros.get(identifier).is_some() =>
                {
                    match self.macros.get(identifier).unwrap() {
                        MacroDefinition::Object(spanneds) => {
                            self.token_trees.prepend_extend(
                                spanneds.iter().map(|token| TokenTree::Token(token)),
                            );
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
            dbg!(&self.token_trees.queue);
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
        macro_name: &'src String,
        parameters: &Spanned<Token>,
        replacment_list: &Vec<&Spanned<Token>>,
    ) {
        self.macros.insert(
            macro_name,
            MacroDefinition::Function {
                parameters: parameters.clone(),
                replacment_list: Self::i_hate_this(replacment_list),
            },
        );
    }
    pub fn handle_define_object<'a>(
        &mut self,
        macro_name: &'src String,
        replacment_list: &'a Vec<&'src Spanned<Token>>,
    ) {
        self.macros.insert(
            macro_name,
            MacroDefinition::Object(replacment_list.to_vec()),
        );
    }
    pub fn i_hate_this<T: Clone>(vec: &Vec<&T>) -> Vec<T> {
        vec.to_vec().iter().map(|tok| (*tok).clone()).collect()
    }
}
