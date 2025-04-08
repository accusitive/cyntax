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
}
#[derive(Debug, Clone)]
pub enum MacroDefinition<'src> {
    Object(ReplacementList<'src>),
    Function { parameter_list: Vec<String>, replacment_list: ReplacementList<'src> },
}

impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn new(source: &'src str, token_trees: PrependingPeekableIterator<I>) -> Self {
        Self {
            source,
            token_trees,
            output: Vec::new(),
            macros: HashMap::new(),
        }
    }
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
            TokenTree::Token(spanned @ span!(Token::Identifier(identifier))) => {
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
            TokenTree::OwnedToken(ref spanned @ span!(Token::Identifier(ref identifier))) => {
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
            ControlLine::Undefine(macro_name) => {
                self.macros.remove(macro_name);
            }
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
