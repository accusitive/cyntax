use std::{
    collections::{HashMap, btree_map::Keys},
    iter::once,
    marker::PhantomData,
    ops::Deref,
};

use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
    sparsechars::{HashedSparseChars, SparseChars},
};
use cyntax_lexer::span;

use crate::tree::{ControlLine, TokenTree};
pub type ReplacementList<'src> = Vec<&'src Spanned<Token>>;
pub enum Macro<'src> {
    Object(&'src ReplacementList<'src>),
    Function(Vec<&'src SparseChars>, &'src ReplacementList<'src>),
}
pub struct ExpandTokens<'src, 'state, TT: AsRef<TokenTree<'src>>, I: Iterator<Item = TT>> {
    pub source: &'src str,
    pub macros: &'state mut HashMap<HashedSparseChars, Macro<'src>>,
    // pub parameters:
    pub token_trees: I,
}
impl<'src, 'state, TT: AsRef<TokenTree<'src>>, I: Iterator<Item = TT>> Iterator
    for ExpandTokens<'src, 'state, TT, I>
{
    type Item = Vec<&'src Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.token_trees.next()?.as_ref() {
            TokenTree::Token(s @ span!(Token::Identifier(identifier))) => {
                match self.macros.get(&identifier.hash(self.source)) {
                    Some(Macro::Object(replacement_list)) => {
                        return Some(replacement_list.to_vec());
                    }
                    Some(Macro::Function(parameters, replacement_list)) => {
                        let next = self.token_trees.next().unwrap();
                        let token = self.expect_tt_token(next).unwrap();
                        let argument_container = self.expect_delimited(token).unwrap();
                        let split_delimited = self.split_delimited(&argument_container.2);
                        // TODO infrastrucure to ignore whitespace, desperately needed
                        // TODO error if delimited brackets are of the wrong kind (ie MACRO_NAME{2,4})
                        let mut map = HashMap::new();
                        for (param, arg) in parameters.iter().zip(split_delimited.iter()) {
                            map.insert(param.hash(self.source), arg);
                        }
                        let new_replacemnt_list: Vec<TokenTree<'src>> = replacement_list
                            .into_iter()
                            .map(|&token| match token {
                                span!(Token::Identifier(i)) => {
                                    if let Some(replacement_list) = map.get(&i.hash(self.source)) {
                                        replacement_list.to_vec()
                                    } else {
                                        vec![token]
                                    }
                                }
                                _ => vec![token],
                            })
                            .flatten()
                            .map(|t| TokenTree::Token(t))
                            .collect::<Vec<_>>();

                        let expanded = ExpandTokens {
                            token_trees: new_replacemnt_list.iter(),
                            source: self.source,
                            macros: self.macros,
                        }
                        .flatten()
                        .collect::<Vec<&'_ Spanned<Token>>>();
                        return Some(expanded);
                    }
                    None => {
                        return Some(vec![s]);
                    }
                }
            }
            TokenTree::Token(spanned) => return Some(vec![spanned]),
            tt @ (TokenTree::IfDef {
                macro_name,
                body,
                opposition,
            }
            | TokenTree::IfNDef {
                macro_name,
                body,
                opposition,
            }) => {
                let invert = matches!(tt, TokenTree::IfNDef { .. });

                let key = macro_name.hash(self.source);
                let mut flag = self.macros.get(&key).is_some();
                if invert {
                    flag = !flag;
                }
                if flag {
                    let toks = ExpandTokens {
                        source: self.source,
                        macros: self.macros,
                        token_trees: body.iter(),
                    }
                    .flatten()
                    .collect::<Vec<_>>();
                    return Some(toks);
                } else {
                    let toks = ExpandTokens {
                        source: self.source,
                        macros: self.macros,
                        token_trees: std::iter::once(opposition.deref()),
                    }
                    .flatten()
                    .collect();
                    return Some(toks);
                }
            }
            TokenTree::If {
                condition,
                body,
                opposition,
            } => todo!(),
            TokenTree::Elif {
                condition,
                body,
                opposition,
            } => todo!(),
            TokenTree::Else { body, opposition } => {
                let toks = ExpandTokens {
                    source: self.source,
                    macros: self.macros,
                    token_trees: body.iter(),
                }
                .flatten()
                .collect();
                if !matches!(opposition.deref(), TokenTree::Endif) {
                    // TODO: Add an error
                }
                return Some(toks);
            }
            TokenTree::Endif => Some(vec![]),
            TokenTree::Directive(control_line) => {
                match control_line {
                    ControlLine::DefineFunction {
                        macro_name,
                        parameters,
                        replacement_list,
                    } => {
                        let key = macro_name.hash(self.source);
                        let parameters = self.parse_parameters(parameters);
                        dbg!(&parameters);
                        self.macros
                            .insert(key, Macro::Function(parameters, replacement_list));

                        Some(vec![])
                    }
                    ControlLine::DefineObject {
                        macro_name,
                        replacement_list,
                    } => {
                        let key = macro_name.hash(self.source);
                        // TODO error handleing here
                        self.macros.insert(key, Macro::Object(replacement_list));

                        Some(vec![])
                    }
                    ControlLine::Undefine(sparse_chars) => todo!(),
                    ControlLine::Error(spanned) => todo!(),
                    ControlLine::Warning(spanned) => todo!(),
                    _ => unreachable!(),
                }
            }
        }
        // self.token_trees.next()
    }
}
fn vec_ref_to_iterator<'src, T>(vec: &'src Vec<T>) -> impl Iterator<Item = &'src T> {
    vec.iter()
}
impl<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> ExpandTokens<'src, 'state, I> {
    pub fn parse_parameters(&self, delimited: &'src Spanned<Token>) -> Vec<&'src SparseChars> {
        match delimited {
            span!(Token::Delimited('(', ')', inner)) => {
                let mut inner_iter = inner.value.iter().peekable();
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
    // [2=5,] -> [[2+5], []]
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
    pub fn enforce_identifier(&self, token: &'src Spanned<Token>) -> &'src SparseChars {
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
    pub fn expect_tt_token(&self, token_tree: &'src TokenTree) -> Option<&'src Spanned<Token>> {
        match token_tree {
            TokenTree::Token(spanned) => Some(*spanned),
            _ => None,
        }
    }
    pub fn expect_delimited(
        &self,
        token: &'src Spanned<Token>,
    ) -> Option<(&char, &char, &'src Spanned<Vec<Spanned<Token>>>)> {
        match token {
            span!(Token::Delimited(open, close, inner)) => Some((open, close, inner)),
            _ => None,
        }
    }
}
