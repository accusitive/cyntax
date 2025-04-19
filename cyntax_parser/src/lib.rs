use crate::ast::*;
use constant::{ConstantParser, IntConstant};
use cyntax_common::{
    ast::*,
    ctx::{Context, string_interner::symbol::SymbolU32},
    spanned::{Location, Spanned},
};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;
use peekmore::PeekMore;
use std::{collections::HashSet, fmt::Debug, str::FromStr, vec::IntoIter};

#[macro_use]
pub mod patterns;
pub mod ast;
pub mod constant;
pub mod decl;
pub mod expr;
pub mod stmt;
pub mod ty;
pub type PResult<T> = Result<T, cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>;

#[derive(Debug)]
struct TokenStream<'src> {
    ctx: &'src mut Context,
    iter: std::vec::IntoIter<Spanned<PreprocessingToken>>,
}
impl<'src> Iterator for TokenStream<'src> {
    type Item = PResult<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next()? {
                span!(span, PreprocessingToken::Identifier(identifier)) => match Keyword::from_str(self.ctx.strings.resolve(identifier).unwrap()) {
                    Ok(kw) => return Some(Ok(Spanned::new(span, Token::Keyword(kw)))),
                    Err(_) => return Some(Ok(Spanned::new(span, Token::Identifier(identifier)))),
                },
                span!(span, PreprocessingToken::BlueIdentifier(identifier)) => return Some(Ok(Spanned::new(span, Token::Identifier(identifier)))),
                span!(span, PreprocessingToken::StringLiteral(string)) => return Some(Ok(Spanned::new(span, Token::StringLiteral(string)))),
                span!(span, PreprocessingToken::CharLiteral(string)) => return Some(Ok(Spanned::new(span, Token::CharLiteral(string)))),
                span!(span, PreprocessingToken::PPNumber(number)) => return Some(self.parse_pp_number(&span, number).map(|ic| Spanned::new(span.clone(), Token::Constant(ic)))),

                span!(span, PreprocessingToken::Punctuator(punc)) => return Some(Ok(Spanned::new(span, Token::Punctuator(punc)))),
                span!(PreprocessingToken::Whitespace(_)) => continue,
                _ => unreachable!(), // span!(PreprocessingToken::)
            }
        }
    }
}

impl<'src> TokenStream<'src> {
    pub fn parse_pp_number(&mut self, location: &Location, number: SymbolU32) -> PResult<IntConstant> {
        ConstantParser::new(location.clone(), self.ctx.strings.resolve(number).unwrap()).lex()
    }
}
type Scope = HashSet<Identifier>;
#[derive(Debug)]
pub struct Parser<'src> {
    pub ctx: &'src mut Context,
    pub diagnostics: Vec<cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>,
    token_stream: peekmore::PeekMoreIterator<IntoIter<PResult<Spanned<Token>>>>,
    last_location: Location,
    scopes: Vec<Scope>,
}
impl<'src> Parser<'src> {
    pub fn new(ctx: &'src mut Context, tokens: Vec<Spanned<PreprocessingToken>>) -> Self {
        let t = TokenStream { ctx, iter: tokens.into_iter() };
        let i = t.collect::<Vec<_>>().into_iter();
        Parser {
            ctx,
            token_stream: i.peekmore(),
            last_location: Location::new(),
            diagnostics: vec![],
            scopes: vec![],
        }
    }
    pub fn next_token(&mut self) -> PResult<Spanned<Token>> {
        match self.token_stream.next() {
            Some(Ok(token)) => {
                self.last_location = token.location.clone();
                Ok(token)
            }
            Some(Err(e)) => Err(e),
            None => Err(SimpleError(self.last_location.clone(), "Unexpected EOF!".to_string()).into_codespan_report()),
        }
    }
    pub fn peek_token_nth(&mut self, n: usize) -> PResult<&Spanned<Token>> {
        match self.token_stream.peek_nth(n) {
            Some(Ok(token)) => {
                self.last_location = token.location.clone();
                Ok(token)
            }
            Some(Err(e)) => Err(e.clone()),
            None => Err(SimpleError(self.last_location.clone(), "Unexpected EOF while peeking!".to_string()).into_codespan_report()),
        }
    }
    pub fn peek_token(&mut self) -> PResult<&Spanned<Token>> {
        self.peek_token_nth(0)
    }

    pub fn eat_if_next(&mut self, t: Token) -> PResult<bool> {
        if self.peek_token()?.value == t {
            self.next_token().unwrap();
            Ok(true)
        } else {
            Ok(false)
        }
    }
    pub fn eat_next(&mut self, t: Token) -> PResult<Option<Spanned<Token>>> {
        if self.peek_token()?.value == t { Ok(Some(self.next_token().unwrap())) } else { Ok(None) }
    }
    pub fn eat_if_same_variant(&mut self, t: Token) -> PResult<bool> {
        if std::mem::discriminant(&self.peek_token()?.value) == std::mem::discriminant(&t) {
            self.next_token().unwrap();
            Ok(true)
        } else {
            Ok(false)
        }
    }
    pub fn peek_matches(&mut self, t: Token) -> PResult<bool> {
        if self.peek_token()?.value == t { Ok(true) } else { Ok(false) }
    }
    pub fn expect_token(&mut self, t: Token, msg: &str) -> PResult<Spanned<Token>> {
        let location = self.last_location.clone();

        match self.next_token() {
            Ok(stoken) if stoken.value == t => Ok(stoken),
            Ok(stoken) => Err(SimpleError(stoken.location, format!("expected {:?}, found {:?}: {msg}", t, stoken.value)).into_codespan_report()),
            Err(e) => Err(SimpleError(location, format!("expected {:?}, found EOF: {:?}", t, e)).into_codespan_report()),
        }
    }
    pub fn maybe_recover<T: Debug, F: FnMut(&mut Self) -> PResult<T>, E: FnOnce(&Self) -> T>(&mut self, mut f: F, e: E, recovery_char: Token) -> T {
        let v = f(self);
        dbg!(&v);
        match v {
            Ok(value) => value,
            Err(err) => {
                self.diagnostics.push(err);
                while let Ok(tok) = self.next_token() {
                    if tok.value == recovery_char {
                        break;
                    }
                }
                e(self)
            }
        }
    }
    pub fn maybe_recover_dont_consume_recovery_char<T, F: FnMut(&mut Self) -> PResult<T>, E: FnMut() -> T>(&mut self, mut f: F, mut e: E, recovery_char: Token) -> T {
        match f(self) {
            Ok(value) => value,
            Err(err) => {
                self.diagnostics.push(err);
                while let Ok(tok) = self.peek_token() {
                    if tok.value == recovery_char {
                        break;
                    } else {
                        self.next_token().unwrap();
                        continue;
                    }
                }
                e()
            }
        }
    }
    pub fn get_declarator_name(&mut self, declarator: &Declarator) -> Identifier {
        match declarator {
            Declarator::Identifier(identifier) => identifier.clone(),
            Declarator::Pointer(_, declarator) => self.get_declarator_name(&declarator.value),
            Declarator::Parenthesized(declarator) => self.get_declarator_name(&declarator.value),
            Declarator::Function(declarator, _) => self.get_declarator_name(&declarator.value),
            Declarator::Array { base, .. } => self.get_declarator_name(&base.value),
            Declarator::Abstract => unreachable!(),
        }
    }
    pub fn declare_typedef(&mut self, init_declarator: &Spanned<InitDeclarator>) {
        let declarator_name = self.get_declarator_name(&init_declarator.value.declarator.value);
        self.scopes.last_mut().unwrap().insert(declarator_name);
        dbg!(&self.scopes);
    }
    pub fn is_typedef(&self, identifier: &Identifier) -> bool {
        self.scopes.iter().any(|scope| scope.contains(identifier))
    }
    pub fn expect_identifier(&mut self) -> PResult<Spanned<SymbolU32>> {
        match self.next_token()? {
            span!(span, Token::Identifier(identifier)) => Ok(Spanned::new(span, identifier)),
            stoken => Err(SimpleError(stoken.location, format!("expected identifier, found {:?}", stoken.value)).into_codespan_report()),
        }
    }
    pub fn consider_comma<T>(&mut self, v: &Vec<T>) -> PResult<bool> {
        Ok(v.len() >= 1 && matches!(self.peek_token()?, span!(Token::Punctuator(Punctuator::Comma))))
    }
    pub fn parse_translation_unit(&mut self) -> PResult<TranslationUnit> {
        self.scopes.push(HashSet::new());
        let mut external_declarations = vec![];
        while let Some(external_declaration) = self.parse_external_declaration()? {
            external_declarations.push(external_declaration);
        }
        self.scopes.pop();
        Ok(TranslationUnit { external_declarations })
    }
    
}
