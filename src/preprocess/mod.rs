use std::{collections::HashMap, slice::Iter};

use ast::{Constant, FloatConstant, IntConstant, Token};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Chars, DisplayStyle,
    },
};
use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    lexer::{HeaderNameKind, Lexer, PreprocessingToken, Punctuator},
    location::{Located, LocationHistory},
    parser::ast::Keyword,
};

pub mod ast;
pub mod eval;
pub mod process;
type PPResult<T> = Result<T, Diagnostic<usize>>;
type TokenStream<'a> = PeekMoreIterator<Iter<'a, LocationHistory<PreprocessingToken>>>;

#[derive(Debug)]
pub struct PreProcessor {
    pub macros: HashMap<String, Macro>,
    pub files: SimpleFiles<String, String>,
    pub file_id: usize,
    pub cursor: LocationHistory<()>,

    pub conditions: Vec<Condition>,
    /// Used while expanding expressions within `if` and `elif` directives, otherwise they might expand into nothing
    pub ignore_conditions: bool,
}

#[derive(Debug, Clone)]
pub enum Macro {
    /// Simple object style macro, contains a simple replacement list
    Object(Vec<LocationHistory<PreprocessingToken>>),
    /// TODO: docs
    Function(Vec<String>, Vec<LocationHistory<PreprocessingToken>>),
}

#[derive(Debug, Clone)]
pub struct Condition {
    value: bool,
}

impl PreProcessor {
    pub fn default_macros() -> HashMap<String, Macro> {
        let mut h = HashMap::new();
        let one = vec![LocationHistory::x(()).same(PreprocessingToken::Number("1".to_string()))];
        h.insert("__STDC__".to_string(), Macro::Object(one.clone()));
        h.insert(
            "__STDC_VERSION__".to_string(),
            Macro::Object(vec![LocationHistory::x(()).same(PreprocessingToken::Number("199901L".to_string()))]),
        );
        h.insert("__STDC_HOSTED__".to_string(), Macro::Object(one.clone()));
        h.insert("__STDC_NO_ATOMICS__".to_string(), Macro::Object(vec![]));
        h.insert("__STDC_NO_COMPLEX__".to_string(), Macro::Object(vec![]));
        h.insert("__STDC_NO_THREADS__".to_string(), Macro::Object(vec![]));
        h.insert("__STDC_NO_VLA__".to_string(), Macro::Object(one.clone()));
        // h.insert("__GNUC__".to_string(), Macro::Object(vec![]));
        // h.insert(
        //     "__cplusplus".to_string(),
        //     Macro::Object(vec![LocationHistory::x(()).same(PreprocessingToken::Number("0".to_string()))]),
        // );
        h.insert("__x86_64__".to_string(), Macro::Object(vec![]));
        h
    }
    pub fn next_token<'a>(&mut self, ts: &mut TokenStream<'a>) -> PPResult<&'a LocationHistory<PreprocessingToken>> {
        let next = match ts.next() {
            Some(e) => {
                self.cursor = e.shell();

                Ok(e)
            }
            None => Err(Diagnostic::error()
                .with_message("EOF next_token")
                .with_labels(vec![Label::primary(self.file_id, self.cursor.location_range()).with_message("Around this area")])),
        };

        // implement token glueing in a similar fashion to handling infix operators in the parser

        next
    }
    pub fn next_non_whitespace_token<'a>(&mut self, ts: &mut TokenStream<'a>) -> PPResult<&'a LocationHistory<PreprocessingToken>> {
        match ts.next() {
            Some(LocationHistory {
                value: PreprocessingToken::Whitespace(_),
                ..
            }) => self.next_non_whitespace_token(ts),
            Some(e) => Ok(e),
            // None => panic!()
            None => Err(Diagnostic::error()
                .with_message("EOF next_non_whitespace_token")
                .with_labels(vec![Label::primary(self.file_id, self.cursor.location_range()).with_message("Around this area")])),
        }
    }
    pub fn peek_non_whitespace_token<'a>(&mut self, ts: &mut TokenStream<'a>, offset: usize) -> PPResult<&'a LocationHistory<PreprocessingToken>> {
        match ts.peek_nth(offset) {
            Some(LocationHistory {
                value: PreprocessingToken::Whitespace(_),
                ..
            }) => self.peek_non_whitespace_token(ts, offset + 1),
            Some(e) => Ok(e),
            None => Err(Diagnostic::error()
                .with_message("EOF peek_token")
                .with_labels(vec![Label::primary(self.file_id, self.cursor.location_range()).with_message("Around this area")])),
        }
    }
    pub fn peek_token<'a>(&mut self, ts: &mut TokenStream<'a>) -> PPResult<&'a LocationHistory<PreprocessingToken>> {
        match ts.peek() {
            Some(e) => Ok(e),
            None => Err(Diagnostic::error()
                .with_message("EOF peek_token")
                .with_labels(vec![Label::primary(self.file_id, self.cursor.location_range()).with_message("Around this area")])),
        }
    }
    pub fn glue_tokens(&mut self, left: &LocationHistory<PreprocessingToken>, right: &LocationHistory<PreprocessingToken>) -> LocationHistory<PreprocessingToken> {
        let left_s = self.stringify_token(left);
        let right_s = self.stringify_token(right);

        let glued = format!("{}{}", left_s, right_s);
        let mut lexer = Lexer::from(glued.as_str());
        let tok = lexer.next_token(&mut glued.chars().peekmore()).unwrap().value.unwrap();

        left.shell().until(&right).shell().same(tok)
    }
    pub fn stringify_token(&mut self, token: &LocationHistory<PreprocessingToken>) -> String {
        match &token.value {
            PreprocessingToken::Identifier(i) => i.to_string(),
            PreprocessingToken::Number(num) => num.to_string(),
            PreprocessingToken::StringLiteral(s) => format!("\"{}\"", s),
            PreprocessingToken::Punctuator(punctuator) => format!("'{}'", punctuator.stringify()),
            _ => unimplemented!(),
        }
    }
    fn consume_if_present(&mut self, ts: &mut TokenStream, expected: &PreprocessingToken) -> PPResult<LocationHistory<bool>> {
        let next = self.peek_token(ts);

        match next {
            Ok(t) if t.value == *expected => {
                let result = t.same(true);
                self.next_token(ts)?;
                Ok(result)
            }
            Ok(t) => Ok(t.same(false)),
            Err(e) => Err(e),
        }
    }
    fn collect_tokens_until_comma(&mut self, ts: &mut TokenStream) -> PPResult<Vec<LocationHistory<PreprocessingToken>>> {
        let mut tokens = vec![];
        let mut parenthesis_balance = 0;
        while let Ok(token) = self.peek_token(ts) {
            if matches!(token.value, PreprocessingToken::Punctuator(Punctuator::LParen)) {
                parenthesis_balance += 1;
            }

            if parenthesis_balance <= 0 && matches!(token.value, PreprocessingToken::Punctuator(Punctuator::RParen)) {
                break;
            }
            if parenthesis_balance <= 0 && matches!(token.value, PreprocessingToken::Punctuator(Punctuator::Comma)) {
                break;
            }
            if matches!(token.value, PreprocessingToken::Punctuator(Punctuator::RParen)) {
                parenthesis_balance -= 1;
            }
            tokens.push(self.next_token(ts)?.clone());
        }
        assert_eq!(parenthesis_balance, 0);
        Ok(tokens)
    }

    fn include(&mut self, hn: &(&String, &HeaderNameKind)) -> Option<String> {
        let paths = ["/usr/include/", "/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/include/", "./"];
        for path in paths {
            if let Ok(s) = std::fs::read_to_string(format!("{}{}", path, hn.0)) {
                return Some(s);
            }
        }
        None
    }
    fn process_token_stream(&mut self, ts: &mut TokenStream) -> Vec<LocationHistory<PreprocessingToken>> {
        let mut accumulator = vec![];

        loop {
            match self.process_token(ts) {
                Ok(Some(token)) => accumulator.extend(token),
                Err(e) => {
                    let writer: StandardStream = StandardStream::stderr(ColorChoice::Always);
                    let mut config = codespan_reporting::term::Config::default();
                    config.display_style = DisplayStyle::Rich;
                    config.chars = Chars::ascii();
                    term::emit(&mut writer.lock(), &config, &self.files, &e).unwrap();
                    panic!();
                }
                Ok(None) => {
                    dbg!(&ts);
                    // assert!(accumulator.len() > 0 );

                    break;
                }
            }
        }
        accumulator
    }
    pub fn process(&mut self, input: Vec<Located<PreprocessingToken>>) -> Vec<LocationHistory<Token>> {
        let tokens = input.iter().map(|t| t.clone().double(self.file_id)).collect::<Vec<_>>();
        let mut token_stream = tokens.iter().peekmore();
        let mut accumulator = vec![];

        loop {
            match self.process_token(&mut token_stream) {
                Ok(Some(tokens)) => {
                    for token in tokens {
                        let parse_token = pp_token_to_token(token);

                        accumulator.push(parse_token);
                    }
                }
                Ok(None) => {
                    break;
                }
                Err(e) => {
                    println!("Failed to process token");
                    let writer: StandardStream = StandardStream::stderr(ColorChoice::Always);
                    let mut config = codespan_reporting::term::Config::default();
                    config.display_style = DisplayStyle::Rich;
                    config.chars = Chars::ascii();
                    term::emit(&mut writer.lock(), &config, &self.files, &e).unwrap();
                    break;
                }
            }
        }
        accumulator
    }

    fn is_active(&self) -> bool {
        // If there are no conditions, it must be active
        if self.conditions.len() == 0 {
            return true;
        }
        // if any of the conditions fail, the block is not active
        for condition in &self.conditions {
            if condition.value == false {
                return false;
            }
        }
        return true;
    }
    fn should_skip(&self) -> bool {
        if self.ignore_conditions {
            return false;
        }
        if self.is_active() == false {
            return true;
        }
        false
    }
}

mod directive;

pub fn pp_token_to_token(token: LocationHistory<PreprocessingToken>) -> LocationHistory<Token> {
    let token_shell = token.shell();
    token_shell.same(match token.value {
        PreprocessingToken::Identifier(i) if Keyword::from_str(&i).is_some() => Token::Keyword(Keyword::from_str(&i).unwrap()),
        PreprocessingToken::Identifier(i) => Token::Identifier(i),
        PreprocessingToken::Number(n) => {
            let constant = if n.contains(".") {
                Constant::Float(FloatConstant::parse(&token_shell.same(n.clone())).unwrap())
            } else {
                Constant::Integer(IntConstant::parse(&token_shell.same(n.clone())).unwrap())
            };
            Token::Constant(constant)
        }
        PreprocessingToken::StringLiteral(s) => Token::StringLiteral(s),
        PreprocessingToken::Punctuator(p) => Token::Punctuator(p),

        token => unimplemented!("processing for token {:?} not implemented", token),
    })
}
