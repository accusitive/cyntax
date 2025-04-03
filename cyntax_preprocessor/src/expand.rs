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

use crate::tree::{ControlLine, IntoTokenTree, TokenTree};
pub type ReplacementList<'src> = Vec<&'src Spanned<Token>>;
#[derive(Debug, Clone)]
pub enum Macro<'src> {
    Object(ReplacementList<'src>),
    Function(Vec<&'src SparseChars>, ReplacementList<'src>),
}
pub struct ExpandTokens<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> {
    pub source: &'src str,
    pub macros: &'state mut HashMap<HashedSparseChars, Macro<'src>>,
    pub current_function_params: Option<HashMap<u64, Vec<&'src Spanned<Token>>>>,
    // pub parameters:
    pub token_trees: I,
}

impl<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> Iterator
    for ExpandTokens<'src, 'state, I>
{
    type Item = Vec<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_tree = self.token_trees.next()?;
        let expanded = self.expand_token(next_tree);
        expanded
    }
}
impl<'src, 'state, I: Iterator<Item = &'src TokenTree<'src>>> ExpandTokens<'src, 'state, I> {
    pub fn expand_token(&mut self, token: &'src TokenTree<'src>) -> Option<Vec<Spanned<Token>>> {
        match token {
            TokenTree::Token(s @ span!(Token::Identifier(identifier))) => {
                if let Some(current_function_params) = &self.current_function_params {
                    if let Some(arg) = current_function_params.get(&identifier.hash(self.source)) {
                        return Some(arg.to_vec().into_iter().cloned().collect());
                    }
                }
                match self.macros.get(&identifier.hash(self.source)) {
                    Some(Macro::Object(replacement_list)) => {
                        let tt = IntoTokenTree {
                            source: self.source,
                            tokens: replacement_list.to_vec().into_iter().peekable(),
                            expecting_opposition: false,
                        }
                        .collect::<Vec<_>>();
                        let mut hm = self.macros.clone();
                        let expander = ExpandTokens {
                            source: self.source,
                            macros: &mut hm,
                            token_trees: tt.iter(),
                            current_function_params: self.current_function_params.clone(),
                        }
                        .flatten()
                        .collect::<Vec<_>>();
                        return Some(expander);
                    }
                    Some(Macro::Function(parameters, replacement_list)) => {
                        let replacement_list: &Vec<&'src Spanned<Token>> = replacement_list;
                        // dbg!(&self.token_trees);
                        let next = self.token_trees.next();
                        if next.is_none() {
                            return Some(vec![(*s).clone()])
                        }
                        let next = next.unwrap();
                        let token = self.expect_tt_token(next).unwrap();
                        let argument_container = self.expect_delimited(token);

                        if argument_container.is_none() {
                            let mut ret = vec![];
                            ret.extend(replacement_list.to_vec().iter().map(|d| (*d).clone()));
                            return Some(ret);
                        }

                        let argument_container = argument_container.unwrap();
                        let split_delimited = self.split_delimited(&argument_container.2);
                        // TODO infrastrucure to ignore whitespace, desperately needed
                        // TODO error if delimited brackets are of the wrong kind (ie MACRO_NAME{2,4})
                        let mut map: HashMap<u64, Vec<&'src Spanned<Token>>> = HashMap::new();
                        for (&param, arg) in parameters.iter().zip(split_delimited.iter()) {
                            map.insert(param.hash(self.source), arg.to_vec());
                        }

                        // let mut expanded_replacement_lsit = vec![];

                        // for token in replacement_list {
                        let itt: Vec<TokenTree<'src>> = IntoTokenTree {
                            source: self.source,
                            tokens: replacement_list.iter().map(|r| *r).peekable(),
                            expecting_opposition: false,
                        }
                        .collect();
                        // The reason this works: because we know that there will never be a directive within the replacement_list, propogating changes to self.macros doesn't matter
                        // should be noted: directives technically can show up, but they aren't lexed into ControlLines so we process it into just a Hash followed by some tokens
                        let mut hm = self.macros.clone();
                        let v = ExpandTokens {
                            source: self.source,
                            macros: &mut hm,
                            token_trees: itt.iter(),
                            current_function_params: Some(map),
                        }
                        .flatten()
                        .collect::<Vec<_>>();

                        // expanded_replacement_lsit.extend(v);
                        // }

                        // let mut replaced_expanded_replacement_list = vec![];
                        // for token in expanded_replacement_lsit {

                        // }
                        return Some(v);
                    }
                    None => {
                        return Some(vec![(*s).clone()]);
                    }
                }
            }
            TokenTree::Token(
                span!(
                    range,
                    Token::Delimited {
                        opener,
                        closer,
                        inner_tokens
                    }
                ),
            ) => {
                let itt: Vec<TokenTree<'src>> = IntoTokenTree {
                    source: self.source,
                    tokens: inner_tokens.value.iter().peekable(),
                    expecting_opposition: false,
                }
                .collect();
                let mut hm = self.macros.clone();
                let v = ExpandTokens {
                    source: self.source,
                    macros: &mut hm,
                    token_trees: itt.iter(),
                    current_function_params: self.current_function_params.clone(),
                }
                .flatten()
                .collect::<Vec<_>>();

                return Some(vec![Spanned::new(
                    range.start..range.end,
                    Token::Delimited {
                        opener: *opener,
                        closer: *closer,
                        inner_tokens: Spanned::new(range.start..range.end, v),
                    },
                )]);
            }
            TokenTree::Token(spanned) => return Some(vec![(*spanned).clone()]),
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
                        current_function_params: self.current_function_params.clone(),
                    }
                    .flatten()
                    .collect::<Vec<_>>();
                    return Some(toks);
                } else {
                    let toks = ExpandTokens {
                        source: self.source,
                        macros: self.macros,
                        token_trees: std::iter::once(opposition.deref()),
                        current_function_params: self.current_function_params.clone(),
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
                    current_function_params: self.current_function_params.clone(),
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
                            .insert(key, Macro::Function(parameters, replacement_list.to_vec()));

                        Some(vec![])
                    }
                    ControlLine::DefineObject {
                        macro_name,
                        replacement_list,
                    } => {
                        let key = macro_name.hash(self.source);
                        // TODO error handleing here
                        self.macros
                            .insert(key, Macro::Object(replacement_list.to_vec()));

                        Some(vec![])
                    }
                    ControlLine::Undefine(sparse_chars) => todo!(),
                    ControlLine::Error(spanned) => todo!(),
                    ControlLine::Warning(spanned) => todo!(),
                    _ => unreachable!(),
                }
            }
        }
    }
    pub fn parse_parameters(&self, delimited: &'src Spanned<Token>) -> Vec<&'src SparseChars> {
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
            span!(Token::Delimited {
                opener: open,
                closer: Some(close),
                inner_tokens
            }) => Some((open, close, inner_tokens)),
            _ => None,
        }
    }
}
