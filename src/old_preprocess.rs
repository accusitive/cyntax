use std::collections::HashMap;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    lexer::{Lexer, PreprocessingToken, Punctuator},
    location::{Located, LocationHistory, Region}, parser::ast::Keyword,
};
#[derive(Debug)]
pub struct PreProcessor<'a> {
    // preprocessing_tokens: Vec<Located<PreprocessingToken>>,
    preprocessing_tokens: PeekMoreIterator<core::slice::Iter<'a, Located<PreprocessingToken>>>,

    macros: HashMap<String, Macro>,
    pub files: SimpleFiles<String, String>,
    file_id: usize,
    conditions: Vec<Condition>,
}
#[derive(Debug, Clone)]
pub enum Macro {
    /// Simple object style macro, contains a simple replacement list
    Object(Vec<LocationHistory<PreprocessingToken>>),
    /// TODO: docs
    Function(Vec<String>, Vec<LocationHistory<PreprocessingToken>>),
}
#[derive(Debug, Clone)]
pub struct Condition {f
    identifier: String,
    kind: ConditionKind,
}
#[derive(Debug, Clone)]
enum ConditionKind {
    IfDef,
    IfNDef,
}

impl<'a> PreProcessor<'a> {
    pub fn new(file_name: String, source: String, value: PeekMoreIterator<core::slice::Iter<'a, Located<PreprocessingToken>>>) -> Self {
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_name, source);
        Self {
            preprocessing_tokens: value,
            files,
            macros: HashMap::new(),
            file_id: file_id,
            conditions: vec![],
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Constant,
    StringLiteral(String),
    Punctuator(Punctuator),
    Number(String),
}

impl Token {
    pub fn describe<'a>(&self) -> String {
        match self {
            Token::Keyword(keyword) => keyword.as_str().to_string(),
            Token::Identifier(_) => "identifier".to_string(),
            Token::Constant => "constant".to_string(),
            Token::StringLiteral(_) => "string literal".to_string(),
            Token::Punctuator(p) => format!("punctuation {:?}", p),
            Token::Number(_) => "number".to_string(),
        }
    }

    pub fn as_identifier(&self) -> Option<&String> {
        if let Self::Identifier(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_number(&self) -> Option<&String> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<'a> PreProcessor<'a> {
    fn process_number(&self, number: &LocationHistory<String>, tokens: &mut Vec<LocationHistory<Token>>) {
        tokens.push(number.same(Token::Number(number.value.clone())));
    }


    fn process_punctuator(&self, punctuator_char: &LocationHistory<Punctuator>, tokens: &mut Vec<LocationHistory<Token>>) {
        tokens.push(punctuator_char.same(Token::Punctuator(punctuator_char.value.clone())));
    }
    fn should_skip(&self) -> bool {
        let mut should_skip = false;
        for condition in &self.conditions {
            let is_defined = self.macros.get(&condition.identifier).is_some();
            if matches!(condition.kind, ConditionKind::IfDef) && !is_defined {
                should_skip = true;
            }
            if matches!(condition.kind, ConditionKind::IfNDef) && is_defined {
                should_skip = true;
            }
        }
        should_skip
    }
    fn process_identifier(&mut self, identifier: &LocationHistory<&String>, tokens: &mut Vec<LocationHistory<Token>>, ) -> usize {
        let mac = self.macros.get(identifier.value).cloned();
        dbg!(&self.macros);
        let mut peek_offset = 0;
        let mut get_next = || {
            let ppt = Some(self.preprocessing_tokens.peek_nth(peek_offset).cloned().unwrap());

            peek_offset+=1;
            dbg!(&peek_offset);
            ppt
        };
        match mac {
            Some(Macro::Function(params, replacement_list)) => {
                // dbg!(&self.preprocessing_tokens.peek().unwrap().start);
                assert_eq!(get_next().unwrap().value, PreprocessingToken::Punctuator(Punctuator::LParen));
                let mut arguments: Vec<Vec<LocationHistory<PreprocessingToken>>> = vec![];
                let mut i = 0;
                let mut paren = 0;
                loop {
                    let next = get_next().unwrap();
                    match next.value {
                        PreprocessingToken::Punctuator(Punctuator::Comma) if paren == 0 => {
                            i += 1;
                            continue;
                        }
                        PreprocessingToken::Punctuator(Punctuator::RParen) if paren == 0 => break,

                        _ => {
                            if matches!(next.value, PreprocessingToken::Punctuator(Punctuator::LParen)) {
                                paren += 1;
                            } else if matches!(next.value, PreprocessingToken::Punctuator(Punctuator::RParen)) {
                                paren -= 1
                            }

                            if arguments.get(i).is_none() {
                                arguments.push(vec![]);
                            }
                            arguments[i].push(next.shell().doubled(self.file_id, next.value.clone()));
                        }
                    }
                }

                let find_param = |target| {
                    let mut index = 0;
                    for param in &params {
                        if param == target {
                            return Some(arguments[index].clone());
                        }
                        index += 1;
                    }
                    None
                };

                let mut patched_replacement_list = vec![];
                for token in &replacement_list {
                    match &token.value {
                        PreprocessingToken::Identifier(identifier) => {
                            // check if it exists as a parameter, if so, return the corresponding argument
                            match find_param(identifier) {
                                Some(argument) => {
                                    patched_replacement_list.extend(argument);
                                }
                                None => {
                                    patched_replacement_list.push(token.clone());
                                }
                            }
                        }
                        _ => {
                            patched_replacement_list.push(token.clone());
                        }
                    }
                }

                for replacement in patched_replacement_list {
                    match &replacement.value {
                        PreprocessingToken::Identifier(identifier) => {
                            // let mut t = Vec::new();
                            self.process_identifier(&replacement.same(identifier), tokens);
                        }
                        PreprocessingToken::Number(number) => {
                            self.process_number(&replacement.same(number.to_string()), tokens);
                        }
                        PreprocessingToken::StringLiteral(s) => {
                            let d = replacement.shell().same(s.to_string());

                            self.process_string_literal(&d, tokens);
                            // self.process_string_literal(&replacement.shell().same(&s.to_string()), tokens);
                        }
                        PreprocessingToken::Punctuator(punctuator) => {
                            self.process_punctuator(&replacement.same(punctuator.clone()), tokens);
                        }
                        _ => unimplemented!(),
                    };
                }
                return peek_offset;

            }
            Some(Macro::Object(replacement_list)) => {
                for replacement in replacement_list {
                    match &replacement.value {
                        PreprocessingToken::Identifier(identifier) => {
                            self.process_identifier(&replacement.same(identifier), tokens);
                        }
                        PreprocessingToken::Number(number) => {
                            self.process_number(&replacement.same(number.to_string()), tokens);
                        }
                        PreprocessingToken::StringLiteral(s) => {
                            let d = replacement.shell().same(s.to_string());

                            self.process_string_literal(&d, tokens);
                            // self.process_string_literal(&replacement.shell().same(&s.to_string()), tokens);
                        }
                        PreprocessingToken::Punctuator(punctuator) => {
                            self.process_punctuator(&replacement.same(punctuator.clone()), tokens);
                        }
                        _ => unimplemented!(),
                    };
                }
            }
            None => tokens.push(identifier.same(Token::Identifier(identifier.value.to_string()))),
        }
        0
    }
    pub fn process_string_literal(&self, string: &LocationHistory<String>, tokens: &mut Vec<LocationHistory<Token>>) {
        tokens.push(string.same(Token::StringLiteral(string.value.clone())));
    }
    pub fn next(&mut self) -> Option<&'a Located<PreprocessingToken>> {
        self.preprocessing_tokens.next()
    }

    pub fn preprocess<'b>(&'b mut self) -> Vec<LocationHistory<Token>> {
        let mut tokens = vec![];
        while let Some(token) = self.next() {
            match &token.value {
                PreprocessingToken::Punctuator(Punctuator::Hash) => {
                    let directive = self.next().unwrap();
                    match directive.value.as_identifier().unwrap().as_ref() {
                        "include" => {
                            let header_name = self.next().unwrap();
                            let header_content = std::fs::read_to_string(format!("/usr/include/{}", header_name.value.as_header_name().unwrap().0));

                            match header_content {
                                Ok(content) => {
                                    let lexer = Lexer::from(content.as_str()).tokenize();
                                    let mut files = self.files.clone();
                                    let file_id = files.add(header_name.value.as_header_name().unwrap().0.to_string(), content);
                                    let mut pp = PreProcessor {
                                        preprocessing_tokens: lexer.iter().peekmore(),
                                        files,
                                        macros: self.macros.clone(),
                                        file_id,
                                        conditions: self.conditions.clone(),
                                    };
                                    tokens.append(&mut pp.preprocess());
                                    self.macros = pp.macros.clone();
                                }
                                Err(e) => {
                                    panic!("Failed to read file {} {}", header_name.value.as_header_name().unwrap().0, e);
                                }
                            }
                        }
                        "define" => {
                            let identifier = self.next().unwrap();
                            if let Some(Located {
                                value: PreprocessingToken::Punctuator(Punctuator::LParen),
                                ..
                            }) = self.preprocessing_tokens.peek()
                            {
                                assert_eq!(self.next().unwrap().value, PreprocessingToken::Punctuator(Punctuator::LParen));

                                let mut params = vec![];
                                let mut replacement_list = vec![];

                                'x: while let Some(token) = self.next() {
                                    if token.value == PreprocessingToken::Punctuator(Punctuator::RParen) {
                                        break;
                                    }
                                    if params.len() > 0 {
                                        if matches!(token.value, PreprocessingToken::Punctuator(Punctuator::Comma)) {
                                            continue 'x;
                                        }
                                    }
                                    // let identifier = self.next().unwrap().value.clone();
                                    let identifier = token.value.clone();
                                    dbg!(&identifier);
                                    let identifier = identifier.as_identifier().unwrap();
                                    params.push(identifier.to_string());
                                }
                                while let Some(token) = self.next() {
                                    if !matches!(token.value, PreprocessingToken::Newline) {
                                        replacement_list.push(token.doubled(self.file_id, token.clone().value));
                                    } else {
                                        break;
                                    }
                                }
                                self.macros
                                    .insert(identifier.value.as_identifier().unwrap().to_string(), Macro::Function(params, replacement_list));
                            } else {
                                let mut replacement_list = vec![];
                                while let Some(token) = self.next() {
                                    if !matches!(token.value, PreprocessingToken::Newline) {
                                        replacement_list.push(token.doubled(self.file_id, token.clone().value));
                                    } else {
                                        break;
                                    }
                                }
                                self.macros.insert(identifier.value.as_identifier().unwrap().to_string(), Macro::Object(replacement_list));
                            }
                        }
                        "undef" => {
                            let identifier = self.next().unwrap();
                            dbg!(&identifier);
                            // panic!();
                            self.macros.remove(identifier.value.as_identifier().unwrap());
                        }
                        "ifdef" => {
                            let identifier = self.next().unwrap();
                            self.conditions.push(Condition {
                                identifier: identifier.value.as_identifier().unwrap().clone(),
                                kind: ConditionKind::IfDef,
                            });
                        }
                        "ifndef" => {
                            let identifier = self.next().unwrap();
                            self.conditions.push(Condition {
                                identifier: identifier.value.as_identifier().unwrap().clone(),
                                kind: ConditionKind::IfNDef,
                            });
                        }
                        "endif" => {
                            self.conditions.pop();
                        }
                        "error" if self.should_skip() => {}
                        "error" => {
                            let mut error_tokens: Vec<LocationHistory<PreprocessingToken>> = vec![];
                            while let Some(token) = self.next() {
                                if !matches!(token.value, PreprocessingToken::Newline) {
                                    error_tokens.push(token.doubled(self.file_id, token.clone().value));
                                } else {
                                    break;
                                }
                            }
                            panic!("Error directive. Tokens: {:#?}, {:#?}, {:#?}", error_tokens, self.macros, self.conditions);
                        }
                        "if" => {
                            let d = Diagnostic::error()
                                .with_message("`#if` directive is not yet implemented")
                                .with_labels(vec![Label::primary(self.file_id, token.start.offset..directive.end.offset)]);
                            let writer: StandardStream = StandardStream::stderr(ColorChoice::Always);
                            let config = codespan_reporting::term::Config::default();
                            term::emit(&mut writer.lock(), &config, &self.files, &d).unwrap();
                            panic!();
                        }
                        s @ _ => {
                            panic!("unhandled directive {}", s)
                        }
                    }
                }
                _ if self.should_skip() => {}
                PreprocessingToken::Identifier(identifier) if Keyword::from_str(identifier).is_some() => {
                    tokens.push(token.doubled(self.file_id, Token::Keyword(Keyword::from_str(identifier).unwrap())))
                }
                PreprocessingToken::Identifier(identifier) => {
                    let skip = self.process_identifier(&token.doubled(self.file_id, identifier), &mut tokens);
                    dbg!(&skip);
                    for _ in 0..skip { self.preprocessing_tokens.next().unwrap();};
                    
                }
                // Handle directives
                PreprocessingToken::Punctuator(c) => self.process_punctuator(&token.doubled(self.file_id, c.clone()), &mut tokens),
                PreprocessingToken::Error(_) => {}
                PreprocessingToken::HeaderName(h, k) => {
                    println!("Somehow reached headername ? {} {:?}", h, k);
                }
                PreprocessingToken::Newline => {}
                PreprocessingToken::StringLiteral(literal) => {
                    self.process_string_literal(&token.doubled(self.file_id, literal.to_string()), &mut tokens);
                }
                PreprocessingToken::Number(n) => {
                    self.process_number(&token.shell().doubled(self.file_id, n.to_string()), &mut tokens);
                }
                x => {
                    println!("{:#?} is not implemented yet", x);
                }
            };
        }

        tokens
    }
}
