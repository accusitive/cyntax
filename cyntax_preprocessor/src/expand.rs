use cyntax_common::{
    ast::{Punctuator, Token},
    spanned::Spanned,
};
use cyntax_errors::{Diagnostic, errors::UnmatchedDelimiter};
use cyntax_errors::{UnwrapDiagnostic, why::Report};
use cyntax_lexer::{lexer::Lexer, span};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    iter::Peekable,
    ops::Range,
    rc::Rc,
    string,
};

use crate::{
    Preprocessor,
    macros::ExpandFunctionMacroControlFlow,
    prepend::PrependingPeekableIterator,
    substitute::ArgumentSubstitutionIterator,
    tree::{ControlLine, InternalLeaf, TokenTree},
};
pub type ReplacementList<'src> = Vec<&'src Spanned<Token>>;
pub type PResult<T> = Result<T, Report>;

#[derive(Debug)]
pub struct Expander<'src, I: Debug + Iterator<Item = TokenTree<'src>>> {
    pub source: &'src str,
    pub token_trees: PrependingPeekableIterator<I>,
    pub output: Vec<Spanned<Token>>,
    pub macros: HashMap<&'src String, MacroDefinition<'src>>,
    pub expanding: HashSet<String>, // pub expanding: HashMap<String, bool>
}
#[derive(Debug, Clone)]
pub enum MacroDefinition<'src> {
    Object(ReplacementList<'src>),
    Function { parameter_list: Vec<String>, replacement_list: ReplacementList<'src> },
}
pub enum ExpandControlFlow<'src> {
    Return(Vec<Spanned<Token>>),
    Rescan(TokenTree<'src>),
    RescanMany(Vec<TokenTree<'src>>),
}
#[derive(Debug)]
pub struct MacroArgument {
    pub unexpanded: Vec<Spanned<Token>>,
    pub expanded: Vec<Spanned<Token>>,
}
impl<'src, I: Debug + Iterator<Item = TokenTree<'src>>> Expander<'src, I> {
    pub fn new(source: &'src str, token_trees: PrependingPeekableIterator<I>) -> Self {
        Self {
            source,
            token_trees,
            output: Vec::new(),
            macros: HashMap::new(),
            expanding: HashSet::new(),
        }
    }
    pub fn expand(&mut self) {
        while let Some(tokens) = self.expand_next(false) {
            dbg!();
            self.output.extend(tokens);
        }
    }
    pub fn expand_next(&mut self, skip_macro_replacement: bool) -> Option<Vec<Spanned<Token>>> {
        dbg!();

        self.token_trees.next().map(|tt| self.fully_expand_token_tree(tt, skip_macro_replacement))
    }
    pub fn fully_expand_token_tree(&mut self, tt: TokenTree<'src>, skip_macro_replacement: bool) -> Vec<Spanned<Token>> {
        match self.expand_token_tree(tt, skip_macro_replacement) {
            ExpandControlFlow::Return(spanneds) => return spanneds,
            ExpandControlFlow::Rescan(token_tree) => {
                self.token_trees.prepend(token_tree);
                self.expand_next(skip_macro_replacement).unwrap()
            }
            ExpandControlFlow::RescanMany(token_trees) => {
                // dbg!(&token_trees);
                self.token_trees.prepend_extend(token_trees.into_iter());
                self.expand_next(skip_macro_replacement).unwrap()
            }
        }
    }
    pub fn expand_token_tree(&mut self, tt: TokenTree<'src>, skip_macro_replacement: bool) -> ExpandControlFlow<'src> {
        // dbg!(&tt);

        let mut output = vec![];
        match tt {
            TokenTree::Internal(InternalLeaf::ExpandingMacro(macro_name)) => {
                self.expanding.insert(macro_name);
            }
            TokenTree::Internal(InternalLeaf::DoneExpandingMacro(macro_name)) => {
                self.expanding.remove(&macro_name);
            }
            TokenTree::Directive(control_line) => {
                self.handle_control_line(control_line);
            }

            // When encountering an opening delimiter, collect all tokens between that and a matching closing delimiter, then reinject it into the token stream to be further processe
            // TokenTree::LexerToken(opening_token @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBrace | Punctuator::LeftBracket))) => {
            //     let delimited_tt = self.collect_until_closing_delimiter(opening_token, true).unwrap();
            //     return ExpandControlFlow::Rescan(TokenTree::PreprocessorToken(delimited_tt));
            // }

            // TokenTree::PreprocessorToken(ref opening_token @ span!(Token::Punctuator(Punctuator::LeftParen | Punctuator::LeftBrace | Punctuator::LeftBracket))) => {
            //     dbg!(opening_token, &self.token_trees);
            //     let delimited_tt = self.collect_until_closing_delimiter(opening_token, true).unwrap();
            //     return ExpandControlFlow::Rescan(TokenTree::PreprocessorToken(delimited_tt));
            // }
            // TokenTree::Token(Token::Punctuator(Punctuator::Hash))
            // When encountering a previously reinjected delimited token stream, expand the body and return a Delimited Token
            // TokenTree::Internal(InternalLeaf::Delimited(opener, closer, body)) => {
            //     let mut expander = Expander {
            //         source: self.source,
            //         expanding: self.expanding.clone(),
            //         macros: self.macros.clone(),
            //         output: Vec::new(),
            //         token_trees: PrependingPeekableIterator::new(body.into_iter()),
            //     };

            //     expander.expand();
            //     let range = opener.range.start..closer.range.end;
            //     return ExpandControlFlow::Return(vec![Spanned::new(
            //         range,
            //         Token::Delimited {
            //             opener: opener,
            //             closer: closer,
            //             inner_tokens: expander.output,
            //         },
            //     )]);
            // }
            TokenTree::Internal(InternalLeaf::MacroExpansion(macro_name, tt)) => {
                let as_tokens = tt.into_iter().map(|token| TokenTree::PreprocessorToken(token)).collect::<Vec<_>>();
                let mut f = vec![TokenTree::Internal(InternalLeaf::ExpandingMacro(macro_name.clone()))];
                dbg!(&as_tokens);

                f.extend(as_tokens);
                f.push(TokenTree::Internal(InternalLeaf::DoneExpandingMacro(macro_name.clone())));
                return ExpandControlFlow::RescanMany(f);
            }
            TokenTree::LexerToken(span!(span, Token::Identifier(identifier))) if self.expanding.contains(identifier) => {
                return ExpandControlFlow::Return(vec![Spanned::new(span.clone(), Token::BlueIdentifier(identifier.clone()))]);
            }
            TokenTree::PreprocessorToken(span!(span, Token::Identifier(identifier))) if self.expanding.contains(&identifier) => {
                return ExpandControlFlow::Return(vec![Spanned::new(span.clone(), Token::BlueIdentifier(identifier.clone()))]);
            }
            TokenTree::LexerToken(span!(span, Token::Identifier(identifier))) if !skip_macro_replacement => {
                return self.handle_identifier(span, identifier);
            }
            TokenTree::PreprocessorToken(span!(span, Token::Identifier(identifier))) if !skip_macro_replacement => {
                return self.handle_identifier(&span, &identifier);
            }
            TokenTree::LexerToken(spanned) => {
                output.push(spanned.clone());
            }
            TokenTree::PreprocessorToken(spanned) => {
                output.push(spanned);
            }

            _ => {}
        }
        ExpandControlFlow::Return(output)
        // Ok(output)
    }
    pub fn handle_identifier(&mut self, span: &Range<usize>, identifier: &String) -> ExpandControlFlow<'src> {
        match self.macros.get(identifier).cloned() {
            Some(MacroDefinition::Object(replacement_list)) => {
                let output = ArgumentSubstitutionIterator {
                    replacements: PrependingPeekableIterator::new(replacement_list.into_iter().filter(|a| !matches!(a, span!(Token::Whitespace(_)))).cloned()),
                    map: HashMap::new(),
                    glue: false,
                    glue_string: String::new(),
                    stringify: false,
                    stringify_string: String::new(),
                    macros: self.get_macro_hashset()
                }
                .flatten()
                .collect::<Vec<_>>();
                return ExpandControlFlow::RescanMany(vec![TokenTree::Internal(InternalLeaf::MacroExpansion(identifier.to_owned(), output))]);

                // return ExpandControlFlow::RescanMany(vec![TokenTree::Internal(InternalLeaf::MacroExpansion(identifier.to_owned(), replacement_list.to_vec().into_iter().cloned().collect()))]);
            }
            Some(MacroDefinition::Function { parameter_list, replacement_list }) => {
                if matches!(
                    self.peek_non_whitespace(),
                    Some(
                        TokenTree::PreprocessorToken(span!(Token::Punctuator(Punctuator::LeftParen) | Token::Delimited { opener: span!('('), .. }))
                            | TokenTree::LexerToken(span!(Token::Punctuator(Punctuator::LeftParen) | Token::Delimited { opener: span!('('), .. }))
                    )
                ) {
                    let (opener, closer, tokens) = self.next_delimited();

                    let arguments = self.split_delimited(tokens.iter());
                    dbg!(&arguments);

                    let mut expanded_args = vec![];

                    for arg in arguments.clone() {
                        let i = arg.into_iter();
                        expanded_args.push(self.expand_arg(i.clone().cloned().collect::<Vec<_>>(), i.into_iter()));
                    }

                    let map = parameter_list.into_iter().zip(expanded_args).collect::<HashMap<_, _>>();

                    let output = ArgumentSubstitutionIterator {
                        replacements: PrependingPeekableIterator::new(replacement_list.into_iter().filter(|a| !matches!(a, span!(Token::Whitespace(_)))).cloned()),
                        map,
                        glue: false,
                        glue_string: String::new(),
                        stringify: false,
                        stringify_string: String::new(),
                        macros: self.get_macro_hashset(),
                    }
                    .flatten()
                    .collect::<Vec<_>>();
                    return ExpandControlFlow::RescanMany(vec![TokenTree::Internal(InternalLeaf::MacroExpansion(identifier.to_owned(), output))]);
                } else {
                    return ExpandControlFlow::Return(vec![Spanned::new(span.clone(), Token::Identifier(identifier.to_string()))]);
                }

                todo!();
            }
            None => ExpandControlFlow::Return(vec![Spanned::new(span.clone(), Token::Identifier(identifier.to_string()))]),
        }
    }

    fn get_macro_hashset(&mut self) -> HashSet<&&String> {
        self.macros.keys().collect::<HashSet<&&String>>()
    }
    pub fn next_delimited(&mut self) -> (Spanned<char>, Spanned<char>, Vec<Spanned<Token>>) {
        let paren = self.next_non_whitespace().unwrap();
        match paren {
            TokenTree::LexerToken(t @ span!(Token::Punctuator(Punctuator::LeftParen))) => {
                let (opener, closer, tokens) = self.collect_until_closing_delimiter(t, true).unwrap().value.as_delimited();
                (opener, closer, tokens)
            }
            TokenTree::PreprocessorToken(ref t @ span!(Token::Punctuator(Punctuator::LeftParen))) => {
                let (opener, closer, tokens) = self.collect_until_closing_delimiter(t, true).unwrap().value.as_delimited();
                (opener, closer, tokens)
            }
            TokenTree::PreprocessorToken(span!(Token::Delimited { opener, closer, inner_tokens })) => (opener.clone(), closer.clone(), inner_tokens.to_vec()),
            _ => panic!(),
        }
    }
    pub fn expand_arg<'a, J: Iterator<Item = &'a Spanned<Token>>>(&mut self, unexpanded: Vec<Spanned<Token>>, arg: J) -> MacroArgument {
        let mut expander = Expander {
            source: self.source,
            expanding: self.expanding.clone(),
            macros: self.macros.clone(),
            output: Vec::new(),
            token_trees: PrependingPeekableIterator::new(arg.map(|t| TokenTree::PreprocessorToken(t.clone())).collect::<Vec<_>>().into_iter()),
        };
        dbg!(&expander.token_trees);
        expander.expand();
        dbg!(&expander.output);
        MacroArgument { expanded: expander.output, unexpanded }
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
                replacement_list: replacment_list.to_vec(),
            },
        );
    }
    pub fn handle_define_object<'func>(&mut self, macro_name: &'src String, replacment_list: &'func Vec<&'src Spanned<Token>>) {
        self.macros.insert(macro_name, MacroDefinition::Object(replacment_list.to_vec()));
    }

    pub fn collect_until_closing_delimiter(&mut self, opening_token: &Spanned<Token>, skip_macro_replacement: bool) -> PResult<Spanned<Token>> {
        let opening_char = opening_token.map_ref(|tok| match tok {
            Token::Punctuator(Punctuator::LeftParen) => '(',
            Token::Punctuator(Punctuator::LeftBracket) => '[',
            Token::Punctuator(Punctuator::LeftBrace) => '{',
            c => unreachable!("{c:?} is not a valid opening char"),
        });
        let expected_closer: char = match opening_char.value {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            _ => unreachable!(),
        };
        let valid_closer = Punctuator::from_char(expected_closer).unwrap();
        let mut inner = vec![];
        let mut end = opening_char.range.end;
        while let Some(token) = self.token_trees.next() {
            if !matches!(token, TokenTree::LexerToken(_) | TokenTree::PreprocessorToken(_)) {
                continue;
            }
            let token = token.as_token();
            end = token.range.end;
            match token {
                span!(rp, Token::Punctuator(ref punc @ (Punctuator::RightParen | Punctuator::RightBracket | Punctuator::RightBrace))) if *punc == valid_closer => {
                    return Ok(Spanned::new(
                        opening_char.range.start..end,
                        Token::Delimited {
                            opener: opening_char,
                            closer: Spanned::new(rp.clone(), expected_closer),
                            inner_tokens: inner,
                        },
                    ));
                }
                span!(token) if token == opening_token.value => {
                    let nested = self.collect_until_closing_delimiter(opening_token, skip_macro_replacement).unwrap();
                    inner.push(nested);
                }
                token => {
                    inner.push(token.clone());
                }
            }
        }
        dbg!(&inner);
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
            Some(TokenTree::LexerToken(span!(Token::Whitespace(_)))) => self.peek_non_whitespace_nth(n + 1),
            Some(TokenTree::PreprocessorToken(span!(Token::Whitespace(_)))) => self.peek_non_whitespace_nth(n + 1),
            Some(tt) => Some(tt),
            None => None,
        }
    }
    pub fn next_non_whitespace(&mut self) -> Option<TokenTree<'src>> {
        let tt = self.token_trees.next()?;

        match tt {
            TokenTree::LexerToken(span!(Token::Whitespace(_))) => self.next_non_whitespace(),
            TokenTree::PreprocessorToken(span!(Token::Whitespace(_))) => self.next_non_whitespace(),

            tt => Some(tt),
        }
    }
}
