use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
use cyntax_errors::{Diagnostic, errors::UnmatchedDelimiter};
use cyntax_errors::{UnwrapDiagnostic, why::Report};
use cyntax_lexer::{lexer::Lexer, span};
use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    iter::Peekable,
    rc::Rc,
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
    pub expanding: HashMap<String, bool>,
}
#[derive(Debug, Clone)]
pub enum MacroDefinition<'src> {
    Object(ReplacementList<'src>),
    Function { parameter_list: Vec<String>, replacment_list: ReplacementList<'src> },
}

impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn expand(&mut self) {
        while let Some(tokens) = self.expand_next(false) {
            dbg!();

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
    pub fn expand_next(&mut self, skip_macro_replacement: bool) -> Option<PResult<Vec<Spanned<Token>>>> {
        dbg!();

        self.token_trees.next().map(|tt| self.expand_token_tree(tt, skip_macro_replacement))
    }
    pub fn expand_token_tree(&mut self, tt: TokenTree<'src>, skip_macro_replacement: bool) -> PResult<Vec<Spanned<Token>>> {
        dbg!();

        let mut output = vec![];
        match tt {
            // When encountering an opening delimiter, collect all tokens between that and a matching closing delimiter, then reinject it into the token stream to be further processe
            TokenTree::Token(opening_token @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBrace | Punctuator::LeftBracket))) => {
                let inner = self.collect_until_closing_delimiter(opening_token, skip_macro_replacement)?;
                self.token_trees.prepend(inner);
            }
            TokenTree::OwnedToken(ref opening_token @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBrace | Punctuator::LeftBracket))) => {
                let inner = self.collect_until_closing_delimiter(opening_token, skip_macro_replacement)?;
                self.token_trees.prepend(inner);
                println!("prepended");
            }
            // TokenTree::Token(Token::Punctuator(Punctuator::Hash))
            // When encountering a previously reinjected delimited token stream, expand the body and return a Delimited Token
            TokenTree::Delimited(opener, closer, body) => {
                // todo: get this to work properly?
                // let delimited_body = body.into_iter().flat_map(|tt| self.expand_token_tree(tt)).flatten().collect();
                let mut delimited_body = vec![];
                for token in body {
                    let token = self.expand_token_tree(token, skip_macro_replacement)?;
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
            TokenTree::Token(spanned @ span!(Token::Identifier(identifier))) if !*self.expanding.get(identifier).unwrap() => {
                dbg!(&"test");
                match self.macros.get(identifier).cloned() {
                    _ if skip_macro_replacement => {
                        output.push(spanned.clone());
                    }
                    Some(MacroDefinition::Function { parameter_list, replacment_list }) => {
                        self.handle_function_style_macro_invocation(identifier.clone(), spanned, &parameter_list, &replacment_list, &mut output);
                    }
                    Some(MacroDefinition::Object(replacement_list)) => {
                        self.token_trees.prepend_extend(replacement_list.iter().map(|token| TokenTree::OwnedToken((*token).clone())));
                    }
                    _ => {
                        output.push(spanned.clone());
                    }
                }
            }
            TokenTree::OwnedToken(ref spanned @ span!(Token::Identifier(ref identifier))) if !*self.expanding.get(identifier).unwrap() => {
                dbg!(&"test");
                match self.macros.get(identifier).cloned() {
                    _ if skip_macro_replacement => {
                        output.push(spanned.clone());
                    }
                    Some(MacroDefinition::Function { parameter_list, replacment_list }) => {
                        self.handle_function_style_macro_invocation(identifier.clone(), spanned, &parameter_list, &replacment_list, &mut output);
                    }
                    Some(MacroDefinition::Object(replacement_list)) => {
                        self.token_trees.prepend_extend(replacement_list.iter().map(|token| TokenTree::OwnedToken((*token).clone())));
                    }
                    _ => {
                        output.push(spanned.clone());
                    }
                }
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
        Ok(output)
    }
    pub fn handle_function_style_macro_invocation<'a>(&mut self, macro_identifier: String, macro_name: &Spanned<Token>, parameter_list: &Vec<String>, replacement_list: &Vec<&'src Spanned<Token>>, output: &mut Vec<Spanned<Token>>) {
       dbg!();
        // if the token invocation is failed, ie, the identifier IS a valid function styler macro, but there is no argument list following it
        let next_non_whitespace = self.peek_non_whitespace();
        if matches!(next_non_whitespace, Some(TokenTree::Token(span!(Token::Punctuator(Punctuator::LeftParen))))) || matches!(next_non_whitespace, Some(TokenTree::OwnedToken(span!(Token::Punctuator(Punctuator::LeftParen))))) {
            self.expanding.insert(macro_identifier.clone(), true);

            let lparen = self.next_non_whitespace().unwrap().as_token();
            let inside = self.collect_until_closing_delimiter(&lparen, true).unwrap();
            let expanded = self.expand_token_tree(inside, true).unwrap();
            let args = if let span!(Token::Delimited { opener: _, closer: _, inner_tokens }) = expanded.first().as_ref().unwrap() {
                self.split_delimited(inner_tokens.iter())
            } else {
                panic!("this should never be reachable")
            };
            // This should raise an error if this expansion is not the result of a previously expanded macro
            if args.len() != parameter_list.len() {
                output.push(macro_name.clone());
                output.extend(expanded);
                return;
            }

            let map: HashMap<_, _> = parameter_list.into_iter().zip(args).collect();
            let substituted = self.substitute_replacement_list_with_arguments(replacement_list.to_vec().into_iter().cloned(), &map);
            self.expanding.insert(macro_identifier, false);

            self.token_trees.prepend_extend(substituted.into_iter());
        } else {
            output.push(macro_name.clone());
            return;
        }
    }
    pub fn substitute_replacement_list_with_arguments<'a, J: Debug + Iterator<Item = Spanned<Token>>>(&mut self, replacement_list: J, parameter_to_arg: &HashMap<&String, ReplacementList<'a>>) -> Vec<TokenTree<'src>> {
        let mut replacement_list = PrependingPeekableIterator::new(replacement_list.filter(|tok| !matches!(tok, span!(Token::Whitespace(_)))));
        dbg!();

        let mut output = vec![];
        while let Some(token) = replacement_list.next() {

            match token {
                span!(Token::Punctuator(Punctuator::Hash)) => {
                    let param = replacement_list.next().expect("`#` must be followed by macro parameter");
                    if let span!(Token::Identifier(identifier)) = param {
                        if let Some(argument_replacement_list) = parameter_to_arg.get(&identifier) {
                            let mut pasted = String::new();
                            self.stringify_tokens(argument_replacement_list.to_vec().into_iter(), &mut pasted);
                            replacement_list.prepend(Spanned::new(0..0, Token::StringLiteral(pasted)));
                        } else {
                            panic!("no param")
                        }
                    } else {
                        panic!("token following # must be an identifier")
                    }
                }
                span!(Token::Identifier(identifier)) if parameter_to_arg.contains_key(&identifier) => {
                    let replacement = parameter_to_arg.get(&identifier).unwrap();
                    replacement_list.prepend_extend(replacement.to_vec().into_iter().cloned());

                }
                token if matches!(replacement_list.peek(), Some(span!(Token::Punctuator(Punctuator::HashHash)))) => {
                    let _hash_hash = replacement_list.next().unwrap();
                    let other = replacement_list.next().unwrap();
                    let range = token.range.start..other.range.end;

                    let mut left = String::new();
                    self.stringify_token(&token, &mut left);
                    let mut right = String::new();
                    dbg!(&other);
                    let expanded_right = self.substitute_replacement_list_with_arguments(std::iter::once(other), parameter_to_arg).into_iter().map(|tt| tt.as_token().into_owned()).collect::<Vec<_>>();
                    dbg!(&expanded_right);
                    self.stringify_tokens(expanded_right.iter(), &mut right);

                    let src = format!("{}{}", left, right);
                    dbg!(&src);
                    let tokens = Lexer::new("test.c", &src).map(|span| Spanned::new(range.clone(), span.value)).collect::<Vec<_>>();
                    dbg!(&tokens);
                    replacement_list.prepend_extend(tokens.into_iter());
                }
                _ => {
                    output.push(token);
                }
            }
        }
        output.into_iter().map(|token| TokenTree::OwnedToken(token)).collect()
    }
    pub fn stringify_tokens<'a, T: Iterator<Item = &'a Spanned<Token>>>(&mut self, tokens: T, s: &mut String) {
        for token in tokens {
            self.stringify_token(token, s);
        }
    }
    pub fn stringify_token(&mut self, token: &Spanned<Token>, s: &mut String) {
        match &token.value {
            Token::Identifier(identifier) => s.push_str(identifier),
            Token::StringLiteral(string) => {
                s.push('"');
                s.push_str(string);
                s.push('"');
            }
            Token::PPNumber(number) => {
                s.push_str(number);
            }
            Token::Whitespace(whitespace) => {
                s.push(match whitespace {
                    cyntax_common::ast::Whitespace::Space => ' ',
                    cyntax_common::ast::Whitespace::Newline => '\n',
                    cyntax_common::ast::Whitespace::Tab => '\t',
                });
            }
            Token::Punctuator(punctuator) => s.push_str(&punctuator.to_string()),
            Token::Delimited { opener, closer, inner_tokens } => {
                s.push(opener.value);
                self.stringify_tokens(inner_tokens.iter(), s);
                s.push(closer.value);
            }
            Token::ControlLine(_) => unreachable!(),
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
        self.expanding.insert(macro_name.clone(), false);
        self.macros.insert(
            macro_name,
            MacroDefinition::Function {
                parameter_list: parameters.clone(),
                replacment_list: replacment_list.to_vec(),
            },
        );
    }
    pub fn handle_define_object<'func>(&mut self, macro_name: &'src String, replacment_list: &'func Vec<&'src Spanned<Token>>) {
        self.expanding.insert(macro_name.clone(), false);
        self.macros.insert(macro_name, MacroDefinition::Object(replacment_list.to_vec()));
    }
    pub fn parse_parameters<'func>(&mut self, parameter_token: Spanned<Token>) -> Vec<String> {
        if let span!(Token::Delimited { opener, closer, inner_tokens }) = parameter_token {
            let no_whitespace = inner_tokens.iter().filter(|token| !matches!(token, span!(Token::Whitespace(_))));
            let parameters = self.split_delimited(no_whitespace);
            dbg!(&parameters);
            for parameter in &parameters {
                if parameter.len() != 1 {
                    panic!("argument must be exactly one identifier");
                }
            }
            let parameters_as_strings = parameters
                .into_iter()
                .map(|argument| match argument.first().unwrap() {
                    span!(Token::Identifier(identifier)) => identifier,
                    // above is a check to make sure that each parameter is exactly one token, and its just an identifier
                    _ => unreachable!(),
                })
                .cloned()
                .collect();
            parameters_as_strings
        } else {
            unreachable!()
        }
    }
    /// Splits a comma-delimited sequence of tokens into groups.
    ///
    /// - `[]` -> `[]`
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

        // manual override for `[]` -> `[]`
        // this can probably be fixed algorithmically
        if elements.len() == 1 && elements.first().unwrap().len() == 0 { vec![] } else { elements }
    }

    pub fn collect_until_closing_delimiter(&mut self, opening_token: &Spanned<Token>, skip_macro_replacement: bool) -> PResult<TokenTree<'src>> {
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
        while let Some(token) = self.expand_next(skip_macro_replacement) {
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
    pub fn peek_non_whitespace(&mut self) -> Option<TokenTree<'src>> {
        self.peek_non_whitespace_nth(0)
    }
    pub fn peek_non_whitespace_nth(&mut self, n: usize) -> Option<TokenTree<'src>> {
        let tt = self.token_trees.peek_nth(n).cloned();

        match tt {
            Some(TokenTree::Token(span!(Token::Whitespace(_)))) => self.peek_non_whitespace_nth(n + 1),
            Some(TokenTree::OwnedToken(span!(Token::Whitespace(_)))) => self.peek_non_whitespace_nth(n + 1),
            Some(tt) => Some(tt),
            None => None,
        }
    }
    pub fn next_non_whitespace(&mut self) -> Option<TokenTree<'src>> {
        let tt = self.token_trees.next()?;

        match tt {
            TokenTree::Token(span!(Token::Whitespace(_))) => self.next_non_whitespace(),
            TokenTree::OwnedToken(span!(Token::Whitespace(_))) => self.next_non_whitespace(),

            tt => Some(tt),
        }
    }
}
