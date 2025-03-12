use std::collections::HashMap;

use ast::{Declarator, DeclaratorKind, ExternalDeclaration, Parameter, TranslationUnit};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Chars, DisplayStyle,
    },
};
use peekmore::PeekMoreIterator;

use crate::{lexer::Punctuator, location::LocationHistory, preprocess::ast::Token};
macro_rules! indloc {
    ($pat: pat) => {
        crate::location::LocationHistory { value: $pat, .. }
    };
}
pub mod ast;
pub mod declaration;
pub mod expression;
pub mod specifier;
pub mod statement;

#[derive(Debug)]
pub struct Parser<'a> {
    pub tokens: PeekMoreIterator<core::slice::Iter<'a, LocationHistory<Token>>>,
    pub files: SimpleFiles<String, String>,
    pub symbol_stack: Vec<HashMap<String, L<Symbol>>>,
    pub location: LocationHistory<()>,
}
#[derive(Debug)]
pub enum Symbol {
    Typename,
    Identifier,
}

type L<T> = LocationHistory<T>;
type ParseResult<T> = Result<T, Diagnostic<usize>>;

impl<'a> Parser<'a> {
    pub fn push_scope(&mut self) {
        self.symbol_stack.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        dbg!(self.symbol_stack.pop().unwrap());
    }
    pub fn default_symbol_stack() -> Vec<HashMap<String, LocationHistory<Symbol>>> {
        let mut stack = vec![];
        let mut global_scope = HashMap::new();

        global_scope.insert("__builtin_va_list".to_string(), LocationHistory::x(()).same(Symbol::Typename));
        stack.push(global_scope);

        stack
    }
    pub fn define_typedef(&mut self, declarator: &L<Declarator>) {
        match &declarator.value.kind {
            DeclaratorKind::Identifier(double_located) => {
                self.symbol_stack
                    .last_mut()
                    .unwrap()
                    .insert(double_located.value.clone(), double_located.same(Symbol::Typename));
            }
            DeclaratorKind::Declarator(_) => todo!(),
            DeclaratorKind::Abstract => {}
        }
    }
    pub fn define_typename(&mut self, type_name: &L<String>) {
        self.symbol_stack.last_mut().unwrap().insert(type_name.value.clone(), type_name.same(Symbol::Typename));
    }
    pub fn define_identifier(&mut self, declarator: &L<Declarator>) -> ParseResult<()> {
        match &declarator.value.kind {
            DeclaratorKind::Identifier(double_located) => {
                if let Some(symbol) = self
                    .symbol_stack
                    .last_mut()
                    .unwrap()
                    .insert(double_located.value.clone(), double_located.same(Symbol::Identifier))
                {
                    return Err(Diagnostic::error().with_message("Identifier already defined").with_labels(vec![
                        Label::primary(declarator.file_id(), declarator.location_range()),
                        Label::secondary(symbol.file_id(), symbol.location_range()).with_message("Previously defined here"),
                    ]));
                } else {
                    return Ok(());
                }
            }
            DeclaratorKind::Declarator(d) => Ok(self.define_identifier(d)?),
            // No error because I call define_identifier recklessly, but don't actually do anything
            DeclaratorKind::Abstract => Ok(()),
        }
    }
    pub fn resolve_typedef(&mut self, declarator: &L<Declarator>) -> bool {
        match &declarator.value.kind {
            DeclaratorKind::Identifier(double_located) => {
                for table in &self.symbol_stack {
                    if table.contains_key(&double_located.value) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }
    pub fn is_typename(&self, token: &Token) -> bool {
        match token {
            Token::Keyword(keyword) => match keyword {
                ast::Keyword::Char
                | ast::Keyword::Double
                | ast::Keyword::Float
                | ast::Keyword::Int
                | ast::Keyword::Long
                | ast::Keyword::Short
                | ast::Keyword::Void
                | ast::Keyword::Bool => true,
                _ => false,
            },
            Token::Identifier(i) if self.resolve_typedef2(i) => true,
            _ => false,
        }
    }
    pub fn resolve_typedef2(&self, identifier: &String) -> bool {
        for table in &self.symbol_stack {
            if let Some(indloc!(Symbol::Typename)) = table.get(identifier) {
                return true;
            }
        }
        false
    }
    pub fn resolve_identifier2(&self, identifier: &String) -> bool {
        for table in &self.symbol_stack {
            if let Some(indloc!(Symbol::Identifier)) = table.get(identifier) {
                return true;
            }
        }
        false
    }
    pub fn parse(&mut self) -> TranslationUnit {
        let mut declarations: Vec<LocationHistory<ExternalDeclaration>> = vec![];

        loop {
            match self.parse_external_declaration(false) {
                Ok(declaration) => match declaration {
                    Some(declaration) => {
                        declarations.push(declaration);
                    }
                    None => {
                        println!("stopped because of the external declaration is nothing");
                        break;
                    }
                },
                Err(e) => {
                    println!("Failed to parse declaration");
                    let writer: StandardStream = StandardStream::stderr(ColorChoice::Always);
                    let mut config = codespan_reporting::term::Config::default();
                    config.display_style = DisplayStyle::Rich;
                    config.chars = Chars::box_drawing();
                    term::emit(&mut writer.lock(), &config, &self.files, &e).unwrap();
                    // panic!("Encountered error when parsing");
                    break;
                }
            }
        }
        TranslationUnit { declarations }
    }
    fn next_token(&mut self) -> ParseResult<&'a L<Token>> {
        match self.tokens.next() {
            Some(t) => {
                self.location = t.shell();
                Ok(t)
            }
            None => Err(Diagnostic::error().with_message("Unexpected of file").with_labels(self.location.generate_location_labels())),
        }
    }
    fn peek_token(&mut self) -> ParseResult<&'a L<Token>> {
        match self.tokens.peek() {
            Some(t) => Ok(t),
            None => Err(Diagnostic::error()
                .with_message("Unexpected end of file while peeking")
                .with_labels(self.location.generate_location_labels())),
        }
    }

    fn parse_identifier(&mut self) -> ParseResult<L<String>> {
        let token = self.next_token()?;
        match &token.value {
            Token::Identifier(identifier) => Ok(token.same(identifier.clone())),
            _ => Err(Diagnostic::error()
                .with_message("Expected Identifier")
                .with_labels(vec![
                    Label::primary(token.file_id(), token.location_range()).with_message(format!("Found {}", token.value.describe()))
                ])
                .with_labels(self.location.generate_location_labels())),
        }
    }
    fn parse_parameter_list(&mut self) -> ParseResult<(Vec<Parameter>, bool)> {
        let mut params = vec![];
        let mut is_variadic = false;
        while let Some(t) = self.tokens.peek() {
            let t_value = t.value.clone();
            if t_value == Token::Punctuator(Punctuator::RParen) {
                break;
            }

            // Don't allow multiple parameters without commas
            // since this only occurs after the first element, this can be interpreted as preceeding every non-first parameter
            if params.len() > 0 {
                self.expect_token_trace(&Token::Punctuator(Punctuator::Comma), line!())?;
            }
            if self.peek_token()?.value == Token::Punctuator(Punctuator::DotDotDot) {
                is_variadic = true;
                self.next_token()?;
                break;
            }
            let specifiers = self.parse_specifier_list();
            let declarator = self.parse_declarator(false)?;

            params.push(Parameter { specifiers, declarator });
        }
        Ok((params, is_variadic))
    }
    fn expect_token(&mut self, expected: &Token) -> ParseResult<&L<Token>> {
        match self.next_token()? {
            t if t.value == *expected => Ok(t),
            real => Err(Diagnostic::error()
                .with_message("Unexpected token")
                .with_labels(self.location.generate_location_labels())
                .with_labels(vec![Label::primary(real.file_id(), real.location_range()).with_message(format!(
                    "Expected {}, found {}",
                    expected.describe(),
                    real.value.describe()
                ))])),
        }
    }
    fn expect_token_trace(&mut self, expected: &Token, line: u32) -> ParseResult<&L<Token>> {
        match self.next_token()? {
            t if t.value == *expected => Ok(t),
            real => Err(Diagnostic::error()
                .with_message("Unexpected token")
                .with_message(format!("trace: called from {}", line))
                .with_labels(self.location.generate_location_labels())
                .with_labels(vec![Label::primary(real.file_id(), real.location_range()).with_message(format!(
                    "Expected {}, found {}",
                    expected.describe(),
                    real.value.describe()
                ))])),
        }
    }
    fn consume_if_present(&mut self, expected: &Token) -> ParseResult<L<bool>> {
        let next = self.peek_token();

        match next {
            Ok(t) if t.value == *expected => {
                let result = t.same(true);
                self.next_token()?;
                Ok(result)
            }
            Ok(t) => Ok(t.same(false)),
            // Maybe this should return Err or none when theres no tokens left, but I would like some better infrastructure to deal with that
            Err(_) => Ok(LocationHistory::x(false)),
        }
    }
}
