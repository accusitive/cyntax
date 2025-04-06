use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
use cyntax_errors::{Diagnostic, errors::UnmatchedDelimiter};
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
    Function { parameter_list: Vec<String>, replacment_list: ReplacementList<'src> },
}

impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn expand(&mut self) {
        while let Some(tokens) = self.expand_next() {
            match tokens {
                Ok(tokens) => {
                    self.output.extend(tokens);
                }
                Err(e) => {
                    panic!("{}", e.with("test.c", self.source));
                }
            }
        }
    }
    // No token: None
    // Error: Some(Err(_))
    pub fn expand_next(&mut self) -> Option<PResult<Vec<Spanned<Token>>>> {
        self.token_trees.next().map(|tt| self.expand_token_tree(tt))
    }
    pub fn expand_token_tree(&mut self, tt: TokenTree<'src>) -> PResult<Vec<Spanned<Token>>> {
        let mut output = vec![];
        match tt {
            // When encountering an opening delimiter, collect all tokens between that and a matching closing delimiter, then reinject it into the token stream to be further processe
            TokenTree::Token(opening_token @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBrace | Punctuator::LeftBracket))) => {
                let inner = self.collect_inside(opening_token)?;
                self.token_trees.prepend(inner);
            }
            TokenTree::OwnedToken(ref opening_token @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBrace | Punctuator::LeftBracket))) => {
                let inner = self.collect_inside(opening_token)?;
                self.token_trees.prepend(inner);
                println!("prepended");
            }
            // When encountering a previously reinjected delimited token stream, expand the body and return a Delimited Token
            TokenTree::Delimited(opener, closer, body) => {
                let mut delimited_body = vec![];
                for token in body {
                    let token = self.expand_token_tree(token)?;
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
                Some(MacroDefinition::Function { parameter_list, replacment_list }) => {
                    self.handle_function_style_macro_invocation(spanned, &mut output);
                }
                Some(MacroDefinition::Object(replacement_list)) => {
                    self.token_trees.prepend_extend(replacement_list.iter().map(|token| TokenTree::OwnedToken((*token).clone())));
                }
                _ => {
                    output.push(spanned.clone());
                }
            },
            TokenTree::OwnedToken(ref spanned @ span!(Token::Identifier(ref idenitfier))) => match self.macros.get(idenitfier) {
                Some(MacroDefinition::Function { parameter_list, replacment_list }) => {
                    unimplemented!()
                }
                Some(MacroDefinition::Object(replacement_list)) => {
                    self.token_trees.prepend_extend(replacement_list.iter().map(|token| TokenTree::OwnedToken((*token).clone())));
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
        Ok(output)
    }
    pub fn handle_function_style_macro_invocation(&mut self, spanned: &Spanned<Token>, output: &mut Vec<Spanned<Token>>) {
        // if the token invocation is failed, ie, the identifier IS a valid function styler macro, but there is no argument list following it
        let mut not_invoked = false;
        if let Some(tt) = self.peek_non_whitespace() {
            let tok = tt.as_token().into_owned();
            match tok {
                span!(Token::Punctuator(Punctuator::LeftParen)) => {
                    self.next_non_whitespace().unwrap();
                    let inside = self.collect_inside(&tok).unwrap();
                    let expanded = self.expand_token_tree(inside).unwrap();
                    assert_eq!(expanded.len(), 1);
                    let expanded = expanded.first().unwrap();
                    if let span!(Token::Delimited { opener: _, closer: _, inner_tokens }) = expanded {
                        let args = self.split_delimited(inner_tokens.iter());
                        dbg!(&args);
                    } else {
                        unreachable!("how?????");
                    }
                }
                _ => {
                    not_invoked = true;
                }
            }
        } else {
            not_invoked = true;
        }

        if not_invoked {
            output.push(spanned.clone());
        }
    }
    pub fn handle_control_line(&mut self, control_line: ControlLine<'src>) {
        dbg!(&control_line);
        match control_line {
            ControlLine::DefineFunction { macro_name, parameters, replacement_list } => self.handle_define_function(macro_name, parameters, &replacement_list),
            ControlLine::DefineObject { macro_name, replacement_list } => self.handle_define_object(macro_name, &replacement_list),
            _ => todo!(),
        }
    }
    pub fn handle_define_function<'func>(&mut self, macro_name: &'src String, parameters: Spanned<Token>, replacment_list: &'func Vec<&'src Spanned<Token>>) {
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
    pub fn parse_parameters<'func>(&mut self, parameters: Spanned<Token>) -> Vec<String> {
        if let span!(Token::Delimited { opener, closer, inner_tokens }) = parameters {
            let no_whitespace = inner_tokens.iter().filter(|token| !matches!(token, span!(Token::Whitespace(_))));
            let arguments = self.split_delimited(no_whitespace);
            for argument in &arguments {
                if argument.len() > 1 {
                    panic!("todo: error about having more than one token in argument");
                }
            }
            let arguments_as_strings = arguments
                .into_iter()
                .map(|argument| match argument.first().unwrap() {
                    span!(Token::Identifier(identifier)) => identifier,
                    // above is a check to make sure that each parameter is exactly one token, and its just an identifier
                    _ => unreachable!(),
                })
                .cloned()
                .collect();
            arguments_as_strings
        } else {
            unreachable!()
        }
    }
    /// Splits a comma-delimited sequence of tokens into groups.
    ///
    /// - `[,,]` -> `[[], [], []]`
    /// - `[2+5]` -> `[[2+5]]`
    /// - `[2+5,]` -> `[[2+5], []]`
    pub fn split_delimited<'a, L: Iterator<Item = &'a Spanned<Token>>>(&self, mut tokens: L) -> Vec<Vec<&'a Spanned<Token>>> {
        let mut elements = vec![];
        let mut current_element = vec![];

        while let Some(token) = tokens.next() {
            if matches!(token, span!(Token::Punctuator(Punctuator::Comma))) {
                elements.push(std::mem::replace(&mut current_element, Vec::new()));
            } else {
                current_element.push(token);
            }
        }
        elements.push(std::mem::replace(&mut current_element, Vec::new()));
        elements
    }

    pub fn collect_inside(&mut self, opening_token: &Spanned<Token>) -> PResult<TokenTree<'src>> {
        let opening_char = opening_token.map_ref(|tok| match tok {
            Token::Punctuator(Punctuator::LeftParen) => '(',
            Token::Punctuator(Punctuator::LeftBracket) => '[',
            Token::Punctuator(Punctuator::LeftBrace) => '{',
            c => unreachable!("{c:?} is not a valid opening char"),
        });
        let expected_closer = match opening_char.value {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            _ => unreachable!(),
        };
        let valid_closer = Punctuator::from_char(expected_closer).unwrap();
        let mut inner = vec![];
        let mut end = opening_char.range.end;
        while let Some(token) = self.expand_next() {
            let token = token?;

            for token in &token {
                end = token.range.end;
                match token {
                    span!(rp, Token::Punctuator(punc @ (Punctuator::RightParen | Punctuator::RightBracket | Punctuator::RightBrace))) if *punc == valid_closer => {
                        return Ok(TokenTree::Delimited(opening_char, Spanned::new(rp.clone(), expected_closer), inner));
                    }
                    token => {
                        inner.push(TokenTree::OwnedToken(token.clone()));
                    }
                }
            }
        }
        Err(UnmatchedDelimiter {
            opening_delimiter_location: opening_token.range.clone(),
            potential_closing_delimiter_location: end,
            closing_delimiter: valid_closer.to_string(),
        }
        .into_why_report())
    }
    pub fn peek_non_whitespace(&mut self) -> Option<TokenTree<'src>>{
        self.peek_non_whitespace_nth(0)
    }
    pub fn peek_non_whitespace_nth(&mut self, n: usize) -> Option<TokenTree<'src>>{
        let tt = self.token_trees.peek_nth(n).cloned();

        match tt {
            Some(TokenTree::Token(span!(Token::Whitespace(_)))) => {
                self.peek_non_whitespace_nth(n+1)
            }
            Some(tt) => Some(tt),
            None => None,
        }
    }
    pub fn next_non_whitespace(&mut self) -> Option<TokenTree<'src>>{

        let tt = self.token_trees.next();

        match tt {
            Some(TokenTree::Token(span!(Token::Whitespace(_)))) => {
                self.peek_non_whitespace()
            }
            Some(tt) => Some(tt),
            None => None,
        }
    }
}
