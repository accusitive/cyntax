use cyntax_common::{
    ast::{PreprocessingToken, Punctuator},
    spanned::Spanned,
};
use cyntax_errors::{Diagnostic, errors::UnmatchedDelimiter};
use cyntax_errors::{errors::SimpleError, why::Report};
use cyntax_lexer::span;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::Range,
};

use crate::{
    macros::MacroParameterList,
    prepend::PrependingPeekableIterator,
    substitute::ArgumentSubstitutionIterator,
    tree::{ControlLine, InternalLeaf, IntoTokenTree, TokenTree},
};
pub type ReplacementList = Vec<Spanned<PreprocessingToken>>;
pub type PResult<T> = Result<T, Report>;

#[derive(Debug)]
pub struct Expander<'src, I: Debug + Iterator<Item = TokenTree>> {
    pub file_name: &'src str,
    pub file_source: &'src str,
    // pub files: Vec<String>,
    pub token_trees: PrependingPeekableIterator<I>,
    pub output: Vec<Spanned<PreprocessingToken>>,
    pub macros: HashMap<String, MacroDefinition>,
    pub expanding: HashSet<String>, // pub expanding: HashMap<String, bool>
}
#[derive(Debug, Clone)]
pub enum MacroDefinition {
    Object(ReplacementList),
    Function { parameter_list: MacroParameterList, replacement_list: ReplacementList },
}
pub enum ExpandControlFlow {
    Return(Vec<Spanned<PreprocessingToken>>),
    Rescan(TokenTree),
    RescanMany(Vec<TokenTree>),
}
#[derive(Debug)]
pub struct MacroArgument {
    pub unexpanded: Vec<Spanned<PreprocessingToken>>,
    pub expanded: Vec<Spanned<PreprocessingToken>>,
}
impl<'src, I: Debug + Iterator<Item = TokenTree>> Expander<'src, I> {
    pub fn new(name: &'src str, source: &'src str, token_trees: PrependingPeekableIterator<I>) -> Self {
        Self {
            file_name: name,
            file_source: source,
            token_trees,
            output: Vec::new(),
            macros: HashMap::new(),
            expanding: HashSet::new(),
            // files: vec![]
        }
    }
    // this recursion isnt that bad, there shouldnt really ever be many BeginExpandingMacro tokens next to eachother. id be shocked to see more than 10
    pub fn next_token_tree(&mut self) -> Option<TokenTree> {
        match self.token_trees.next() {
            Some(TokenTree::Internal(InternalLeaf::BeginExpandingMacro(mac))) => {
                self.expanding.insert(mac);
                self.next_token_tree()
            }
            Some(TokenTree::Internal(InternalLeaf::FinishExpandingMacro(mac))) => {
                self.expanding.remove(&mac);
                self.next_token_tree()
            }
            Some(tt) => Some(tt),
            None => None,
        }
    }
    pub fn expand(&mut self) -> PResult<()> {
        while let Some(tokens) = self.expand_next(false)? {
            self.output.extend(tokens);
        }
        Ok(())
    }
    pub fn expand_next(&mut self, skip_macro_replacement: bool) -> PResult<Option<Vec<Spanned<PreprocessingToken>>>> {
        match self.next_token_tree() {
            Some(tt) => {
                let expanded = self.fully_expand_token_tree(tt, skip_macro_replacement)?;
                Ok(Some(expanded))
            }
            None => Ok(None),
        }
    }
    pub fn fully_expand_token_tree(&mut self, tt: TokenTree, skip_macro_replacement: bool) -> PResult<Vec<Spanned<PreprocessingToken>>> {
        match self.expand_token_tree(tt, skip_macro_replacement)? {
            ExpandControlFlow::Return(spanneds) => return Ok(spanneds),
            ExpandControlFlow::Rescan(token_tree) => {
                self.token_trees.prepend(token_tree);
                Ok(self.expand_next(skip_macro_replacement)?.unwrap())
            }
            ExpandControlFlow::RescanMany(token_trees) => {
                assert!(token_trees.len() > 0);
                dbg!(&token_trees);
                self.token_trees.prepend_extend(token_trees.into_iter());
                let mut result = Vec::new();
                let tokens = self.expand_next(skip_macro_replacement)?.unwrap();
                result.extend(tokens);
                Ok(result)
            }
        }
    }
    pub fn expand_token_tree(&mut self, tt: TokenTree, skip_macro_replacement: bool) -> PResult<ExpandControlFlow> {
        let mut output = vec![];
        match tt {
            TokenTree::Directive(ControlLine::Error(range, message)) => {
                return Err(cyntax_errors::errors::ErrorDirective(range, message).into_why_report());
            }
            TokenTree::Directive(ControlLine::Include(header_name)) => match header_name {
                crate::tree::HeaderName::Q(file_name) => {
                    let content = std::fs::read_to_string(file_name.clone()).unwrap();
                    let lexer = cyntax_lexer::lexer::Lexer::new(&file_name, &content);
                    let toks = lexer.collect::<Vec<_>>();
                    let trees = IntoTokenTree {
                        source: &content,
                        tokens: toks.iter().peekable(),
                        expecting_opposition: false,
                    }
                    .collect::<Vec<TokenTree>>();

                    return Ok(ExpandControlFlow::RescanMany(trees));
                }
                crate::tree::HeaderName::H(tokens) => {
                    let first = tokens.first().unwrap();
                    let last = tokens.last().unwrap();
                    let range = first.range.start..last.range.end;
                    let file_name = &self.file_source[range];

                    let content = std::fs::read_to_string(file_name).unwrap();
                    let lexer = cyntax_lexer::lexer::Lexer::new(&file_name, &content);
                    let toks = lexer.collect::<Vec<_>>();
                    let trees = IntoTokenTree {
                        source: &content,
                        tokens: toks.iter().peekable(),
                        expecting_opposition: false,
                    }
                    .collect::<Vec<TokenTree>>();

                    return Ok(ExpandControlFlow::RescanMany(trees));
                }
            },
            TokenTree::Directive(control_line) => {
                self.handle_control_line(control_line)?;
            }
            TokenTree::Internal(InternalLeaf::MacroExpansion(macro_name, tt)) => {
                let as_tokens = tt.into_iter().map(|token| TokenTree::PreprocessorToken(token)).collect::<Vec<_>>();
                let mut f = vec![];
                f.push(TokenTree::Internal(InternalLeaf::BeginExpandingMacro(macro_name.clone())));
                f.extend(as_tokens);
                f.push(TokenTree::Internal(InternalLeaf::FinishExpandingMacro(macro_name.clone())));
                return Ok(ExpandControlFlow::RescanMany(f));
            }
            TokenTree::PreprocessorToken(span!(span, PreprocessingToken::Identifier(identifier))) if self.expanding.contains(&identifier) => {
                dbg!(&identifier, &self.expanding);
                return Ok(ExpandControlFlow::Return(vec![Spanned::new(span.clone(), PreprocessingToken::BlueIdentifier(identifier.clone()))]));
            }
            TokenTree::PreprocessorToken(span!(span, PreprocessingToken::Identifier(identifier))) if !skip_macro_replacement => {
                return self.handle_identifier(&span, &identifier);
            }
            TokenTree::PreprocessorToken(spanned) => {
                output.push(spanned);
            }

            TokenTree::IfDef { macro_name, body, opposition } => {
                if self.macros.contains_key(&macro_name) {
                    return Ok(ExpandControlFlow::RescanMany(body));
                } else {
                    return Ok(ExpandControlFlow::Rescan(*opposition));
                }
            }
            TokenTree::IfNDef { macro_name, body, opposition } => {
                if !self.macros.contains_key(&macro_name) {
                    return Ok(ExpandControlFlow::RescanMany(body));
                } else {
                    return Ok(ExpandControlFlow::Rescan(*opposition));
                }
            }
            TokenTree::Else { body, opposition } => {
                dbg!(&opposition);
                return Ok(ExpandControlFlow::RescanMany(body));
            }
            _ => {
                println!("warning: unhandled tt {:#?}", tt)
            }
        }
        Ok(ExpandControlFlow::Return(output))
        // Ok(output)
    }
    pub fn handle_identifier(&mut self, span: &Range<usize>, identifier: &str) -> PResult<ExpandControlFlow> {
        match self.macros.get(identifier).cloned() {
            Some(MacroDefinition::Object(replacement_list)) => {
                let output = ArgumentSubstitutionIterator {
                    replacements: PrependingPeekableIterator::new(replacement_list.into_iter().filter(|a| !matches!(a, span!(PreprocessingToken::Whitespace(_))))),
                    map: HashMap::new(),
                    variadic_args: vec![],
                    is_variadic: false,
                    glue_next_token: false,
                    glue_string: String::new(),
                    stringify_next_token: false,
                    stringify_string: String::new(),
                }
                .flatten()
                .collect::<Vec<_>>();
                dbg!(&identifier, &output);
                return Ok(ExpandControlFlow::Rescan(TokenTree::Internal(InternalLeaf::MacroExpansion(identifier.to_owned(), output))));
            }
            Some(MacroDefinition::Function { parameter_list, replacement_list }) => {
                let nws = self.peek_non_whitespace();
                if matches!(
                    nws,
                    Some(TokenTree::PreprocessorToken(span!(PreprocessingToken::Punctuator(Punctuator::LeftParen) | PreprocessingToken::Delimited { opener: span!('('), .. })))
                ) {
                    let (opener, closer, tokens) = self.next_delimited();
                    let arguments = self.split_delimited(tokens.iter());
                    if (arguments.len() < parameter_list.parameters.len()) || (!parameter_list.variadic && (arguments.len() > parameter_list.parameters.len())) {
                        return Err(SimpleError(opener.range.start..closer.range.end, format!("Expected {} parameters, found {}", parameter_list.parameters.len(), arguments.len())).into_why_report());
                    }
                    dbg!(&arguments);

                    let mut expanded_args = vec![];

                    for arg in arguments {
                        let i = arg.value.clone().into_iter();
                        expanded_args.push(MacroArgument {
                            unexpanded: arg.value.into_iter().cloned().collect(),
                            expanded: self.expand_arg(i.into_iter()),
                        });
                    }
                    let mut eai = expanded_args.into_iter();
                    let map = parameter_list.parameters.into_iter().zip(eai.by_ref()).collect::<HashMap<_, _>>();

                    let extras = eai.collect::<Vec<_>>();

                    let output = ArgumentSubstitutionIterator {
                        replacements: PrependingPeekableIterator::new(replacement_list.into_iter().filter(|a| !matches!(a, span!(PreprocessingToken::Whitespace(_))))),
                        map,
                        variadic_args: extras,
                        is_variadic: parameter_list.variadic,
                        glue_next_token: false,
                        glue_string: String::new(),
                        stringify_next_token: false,
                        stringify_string: String::new(),
                    }
                    .flatten()
                    .collect::<Vec<_>>();

                    return Ok(ExpandControlFlow::RescanMany(vec![TokenTree::Internal(InternalLeaf::MacroExpansion(identifier.to_owned(), output))]));
                } else {
                    return Ok(ExpandControlFlow::Return(vec![Spanned::new(span.clone(), PreprocessingToken::Identifier(identifier.to_string()))]));
                }
            }
            None => Ok(ExpandControlFlow::Return(vec![Spanned::new(span.clone(), PreprocessingToken::Identifier(identifier.to_string()))])),
        }
    }

    pub fn next_delimited(&mut self) -> (Spanned<char>, Spanned<char>, Vec<Spanned<PreprocessingToken>>) {
        let paren = self.next_non_whitespace().unwrap();
        match paren {
            TokenTree::PreprocessorToken(ref t @ span!(PreprocessingToken::Punctuator(Punctuator::LeftParen))) => {
                let (opener, closer, tokens) = self.collect_until_closing_delimiter(t, true).unwrap().value.as_delimited();
                (opener, closer, tokens)
            }
            // TokenTree::LexerToken(span!(Token::Delimited { opener, closer, inner_tokens })) => (opener.clone(), closer.clone(), inner_tokens.to_vec()),
            TokenTree::PreprocessorToken(span!(PreprocessingToken::Delimited { opener, closer, inner_tokens })) => (opener.clone(), closer.clone(), inner_tokens.to_vec()),
            t => panic!("ooking for delimited but found {:#?}", t),
        }
    }
    pub fn expand_arg<'a, J: Iterator<Item = &'a Spanned<PreprocessingToken>>>(&mut self, arg: J) -> Vec<Spanned<PreprocessingToken>> {
        let mut expander = Expander {
            file_name: self.file_name,
            file_source: self.file_source,
            expanding: self.expanding.clone(),
            macros: self.macros.clone(),
            output: Vec::new(),
            token_trees: PrependingPeekableIterator::new(arg.map(|t| TokenTree::PreprocessorToken(t.clone())).collect::<Vec<_>>().into_iter()),
        };
        dbg!(&expander.token_trees);
        expander.expand().unwrap();
        expander.output
    }

    pub fn handle_control_line(&mut self, control_line: ControlLine) -> PResult<()> {
        dbg!(&control_line);
        match control_line {
            ControlLine::DefineFunction { macro_name, parameters, replacement_list } => {
                self.handle_define_function(macro_name, parameters, &replacement_list)?;
            }
            ControlLine::DefineObject { macro_name, replacement_list } => {
                self.handle_define_object(macro_name, &replacement_list);
            }
            ControlLine::Undefine(macro_name) => {
                self.macros.remove(&macro_name);
            }
            _ => todo!(),
        }
        Ok(())
    }
    pub fn handle_define_function<'func>(&mut self, macro_name: String, parameters: Spanned<PreprocessingToken>, replacment_list: &'func Vec<Spanned<PreprocessingToken>>) -> PResult<()> {
        let parameters = self.parse_parameters(parameters)?;
        self.macros.insert(
            macro_name,
            MacroDefinition::Function {
                parameter_list: parameters.clone(),
                replacement_list: replacment_list.to_vec(),
            },
        );
        Ok(())
    }
    pub fn handle_define_object<'func>(&mut self, macro_name: String, replacment_list: &'func Vec<Spanned<PreprocessingToken>>) {
        self.macros.insert(macro_name, MacroDefinition::Object(replacment_list.to_vec()));
    }

    pub fn collect_until_closing_delimiter(&mut self, opening_token: &Spanned<PreprocessingToken>, skip_macro_replacement: bool) -> PResult<Spanned<PreprocessingToken>> {
        let opening_char = opening_token.map_ref(|tok| match tok {
            PreprocessingToken::Punctuator(Punctuator::LeftParen) => '(',
            PreprocessingToken::Punctuator(Punctuator::LeftBracket) => '[',
            PreprocessingToken::Punctuator(Punctuator::LeftBrace) => '{',
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
        while let Some(token) = self.next_token_tree() {
            if !matches!(token, TokenTree::PreprocessorToken(_)) {
                continue;
            }
            let token = token.as_token();
            end = token.range.end;
            match token {
                span!(rp, PreprocessingToken::Punctuator(ref punc @ (Punctuator::RightParen | Punctuator::RightBracket | Punctuator::RightBrace))) if *punc == valid_closer => {
                    return Ok(Spanned::new(
                        opening_char.range.start..end,
                        PreprocessingToken::Delimited {
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
    pub fn peek_non_whitespace(&mut self) -> Option<TokenTree> {
        self.peek_non_whitespace_nth(0)
    }
    pub fn peek_non_whitespace_nth(&mut self, n: usize) -> Option<TokenTree> {
        let tt = self.token_trees.peek_nth(n).cloned();

        match tt {
            Some(TokenTree::Internal(InternalLeaf::BeginExpandingMacro(_))) => self.peek_non_whitespace_nth(n + 1),
            Some(TokenTree::Internal(InternalLeaf::FinishExpandingMacro(_))) => self.peek_non_whitespace_nth(n + 1),
            Some(TokenTree::PreprocessorToken(span!(PreprocessingToken::Whitespace(_)))) => self.peek_non_whitespace_nth(n + 1),
            Some(_) => tt,
            None => None,
        }
    }
    pub fn next_non_whitespace(&mut self) -> Option<TokenTree> {
        let tt = self.next_token_tree()?;

        match tt {
            TokenTree::PreprocessorToken(span!(PreprocessingToken::Whitespace(_))) => self.next_non_whitespace(),

            tt => Some(tt),
        }
    }
}
