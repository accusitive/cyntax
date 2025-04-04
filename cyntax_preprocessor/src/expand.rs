use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    iter::Peekable,
};

use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
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
    pub fn peek(&mut self) -> Option<&I::Item> {
        if let Some(front) = self.queue.front() {
            return Some(front);
        } else {
            self.inner.peek()
        }
    }
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
        parameter_list: Vec<&'src String>,
        replacment_list: ReplacementList<'src>,
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
                        MacroDefinition::Object(replacement_list) => {
                            self.token_trees.prepend_extend(
                                replacement_list.iter().map(|token| TokenTree::Token(token)),
                            );
                        }
                        MacroDefinition::Function {
                            parameter_list: parameters,
                            replacment_list,
                        } => {
                            let argument_list = self.token_trees.next().unwrap();
                            let argument_list = self.expect_tt_token(argument_list).unwrap();
                            let argument_container = self.expect_delimited(argument_list).unwrap();
                            let split_delimited = self.split_delimited(&argument_container.2);


                            let mut map: HashMap<String, Vec<Spanned<Token>>> = HashMap::new();
                            for (&param, arg) in parameters.iter().zip(split_delimited.iter()) {
                                map.insert(param.to_string(), Self::i_hate_this(arg));
                            }

                            // TODO: make this work recursively, im pretty sure it can be extracted into a function
                            // like self.replace_params_with_args(ts: TokenStream) -> TokenStream
                            // then it replaces all identifiers with params,
                            // and delimiters it recursively calls itself

                            // GL future me

                            // let replacement_list = replacment_list
                            //     .iter()
                            //     .map(|token| match token {
                            //         span!(Token::Delimited { opener, closer, inner_tokens }) => {
                            //             // 
                            //         }
                            //         span!(Token::Identifier(i)) => {
                            //             if let Some(e) = map.get(i) {
                            //                 e.to_vec()
                            //             } else {
                            //                 vec![(*token).clone()]
                            //             }
                            //         }
                            //         _ => vec![(*token).clone()]
                            //     })
                            //     .flatten()
                            //     .map(|token| TokenTree::OwnedToken(token));

                            self.token_trees.prepend_extend(replacement_list);

                            dbg!(&argument_list);
                        }
                    }
                }
                TokenTree::Token(spanned) => {
                    self.output.push(spanned.clone());
                }
                TokenTree::OwnedToken(spanned) => {
                    self.output.push(spanned);
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
    pub fn handle_define_function<'func>(
        &mut self,
        macro_name: &'src String,
        parameters: &'src Spanned<Token>,
        replacment_list: &'func Vec<&'src Spanned<Token>>,
    ) {
        let parameters = self.parse_parameters(parameters);
        self.macros.insert(
            macro_name,
            MacroDefinition::Function {
                parameter_list: parameters.clone(),
                replacment_list: replacment_list.to_vec(),
            },
        );
    }
    pub fn handle_define_object<'func>(
        &mut self,
        macro_name: &'src String,
        replacment_list: &'func Vec<&'src Spanned<Token>>,
    ) {
        self.macros.insert(
            macro_name,
            MacroDefinition::Object(replacment_list.to_vec()),
        );
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
    pub fn expect_tt_token(&self, token_tree: TokenTree<'src>) -> Option<&'src Spanned<Token>> {
        match token_tree {
            TokenTree::Token(spanned) => Some(spanned),
            _ => None,
        }
    }
    pub fn parse_parameters(&self, delimited: &'src Spanned<Token>) -> Vec<&'src String> {
        match delimited {
            span!(Token::Delimited {
                opener: '(',
                closer: Some(')'),
                inner_tokens
            }) => {
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
    pub fn split_delimited(
        &self,
        tokens: &'src Spanned<Vec<Spanned<Token>>>,
    ) -> Vec<Vec<&'src Spanned<Token>>> {
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
    pub fn expect_delimited(
        &self,
        token: &'src Spanned<Token>,
    ) -> Option<(&char, &char, &'src Spanned<Vec<Spanned<Token>>>)> {
        match token {
            span!(Token::Delimited {
                opener: open,
                closer: Some(close),
                inner_tokens
            }) => Some((open, close, inner_tokens)),
            _ => None,
        }
    }
    pub fn next_non_whitespace(&mut self) -> Option<TokenTree<'src>> {
        match self.token_trees.next()? {
            TokenTree::OwnedToken(span!(Token::Whitespace(_)))
            | TokenTree::Token(span!(Token::Whitespace(_))) => self.next_non_whitespace(),

            token => {
                return Some(token);
            }
        }
    }
}
