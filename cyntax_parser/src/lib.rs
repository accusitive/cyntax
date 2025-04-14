use crate::ast::*;
use cyntax_common::{
    ast::*,
    spanned::{Span, Spanned},
};
use cyntax_errors::{Diagnostic, errors::SimpleError, why::Report};
use cyntax_lexer::span;
use peekmore::{PeekMore, PeekMoreIterator};
use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, Range},
    str::FromStr,
};

#[macro_use]
pub mod patterns;
pub mod ast;
pub mod decl;
pub mod expr;
pub mod stmt;
pub type PResult<T> = Result<T, cyntax_errors::why::Report>;

#[derive(Debug)]
struct TokenStream {
    iter: std::vec::IntoIter<Spanned<PreprocessingToken>>,
}
impl Iterator for TokenStream {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next()? {
                span!(span, PreprocessingToken::Identifier(identifier)) if Keyword::from_str(&identifier).is_ok() => return Some(Spanned::new(span, Token::Keyword(Keyword::from_str(&identifier).unwrap()))),
                span!(span, PreprocessingToken::Identifier(identifier)) => return Some(Spanned::new(span, Token::Identifier(identifier))),
                span!(span, PreprocessingToken::BlueIdentifier(identifier)) => return Some(Spanned::new(span, Token::Identifier(identifier))),
                span!(span, PreprocessingToken::StringLiteral(string)) => return Some(Spanned::new(span, Token::StringLiteral(string))),
                span!(span, PreprocessingToken::CharLiteral(string)) => return Some(Spanned::new(span, Token::CharLiteral(string))),
                span!(span, PreprocessingToken::PPNumber(number)) => todo!(),
                span!(span, PreprocessingToken::Punctuator(punc)) => return Some(Spanned::new(span, Token::Punctuator(punc))),
                span!(PreprocessingToken::Whitespace(_)) => continue,
                _ => unreachable!(), // span!(PreprocessingToken::)
            }
        }
    }
}
type Scope = HashSet<String>;
#[derive(Debug)]
pub struct Parser {
    pub diagnostics: Vec<Report>,

    token_stream: peekmore::PeekMoreIterator<TokenStream>,
    last_location: Range<usize>,
    scopes: Vec<Scope>,
}
impl Parser {
    pub fn new(tokens: Vec<Spanned<PreprocessingToken>>) -> Self {
        Parser {
            token_stream: TokenStream { iter: tokens.into_iter() }.peekmore(),
            last_location: 0..0,
            diagnostics: vec![],
            scopes: vec![],
        }
    }
    pub fn next_token(&mut self) -> PResult<Spanned<Token>> {
        match self.token_stream.next() {
            Some(token) => {
                self.last_location = token.range.clone();
                Ok(token)
            }
            None => Err(SimpleError {
                0: self.last_location.clone(),
                1: "Unexpected EOF!".to_string(),
            }
            .into_why_report()),
        }
    }
    pub fn peek_token(&mut self) -> PResult<&Spanned<Token>> {
        match self.token_stream.peek() {
            Some(token) => {
                self.last_location = token.range.clone();
                Ok(token)
            }
            None => Err(SimpleError {
                0: self.last_location.clone(),
                1: "Unexpected EOF!".to_string(),
            }
            .into_why_report()),
        }
    }
    pub fn eat_if_next(&mut self, t: Token) -> PResult<bool> {
        if self.peek_token()?.value == t {
            self.next_token().unwrap();
            Ok(true)
        } else {
            Ok(false)
        }
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
    pub fn expect_token(&mut self, t: Token) -> PResult<Spanned<Token>> {
        match self.next_token()? {
            stoken if stoken.value == t => Ok(stoken),
            stoken => Err(SimpleError(stoken.range, format!("expected {:?}, found {:?}", t, stoken.value)).into_why_report()),
        }
    }
    pub fn maybe_recover<T, F: FnMut(&mut Self) -> PResult<T>, E: FnMut() -> T>(&mut self, mut f: F, mut e: E, recovery_char: Token) -> T {
        match f(self) {
            Ok(value) => value,
            Err(err) => {
                self.diagnostics.push(err);
                while let Ok(tok) = self.next_token() {
                    if tok.value == recovery_char {
                        break;
                    }
                }
                e()
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
                    }
                }
                e()
            }
        }
    }
    pub fn get_declarator_name(&mut self, declarator: &Declarator) -> String {
        match declarator {
            Declarator::Identifier(identifier) => identifier.clone(),
            Declarator::Pointer(_, declarator) => self.get_declarator_name(&declarator.value),
            Declarator::Parenthesized(declarator) => self.get_declarator_name(&declarator.value),
            Declarator::Function(declarator, _) => self.get_declarator_name(&declarator.value),
            Declarator::Abstract => unreachable!(),
        }
    }
    pub fn declare_typedef(&mut self, init_declarator: &InitDeclarator) {
        let declarator_name = self.get_declarator_name(&init_declarator.declarator.value);
        self.scopes.last_mut().unwrap().insert(declarator_name);
        dbg!(&self.scopes);
    }
    pub fn is_typedef(&self, identifier: &str) -> bool {
        for scope in &self.scopes {
            if scope.contains(identifier) {
                return true;
            }
        }
        return false;
    }
    pub fn expect_identifier(&mut self) -> PResult<Spanned<String>> {
        match self.next_token()? {
            span!(span, Token::Identifier(identifier)) => Ok(Spanned::new(span, identifier)),
            stoken => Err(SimpleError(stoken.range, format!("expected identifier, found {:?}", stoken.value)).into_why_report()),
        }
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

    pub fn parse_struct_type_specifier(&mut self) -> PResult<ast::TypeSpecifier> {
        let name = if let span!(Token::Identifier(identifer)) = self.peek_token()? { Some(identifer.clone()) } else { None };
        if name.is_some() {
            self.next_token()?;
        }

        if self.eat_if_next(Token::Punctuator(Punctuator::LeftBrace))? {
            let declarations = self.parse_struct_declaration_list()?;

            self.expect_token(Token::Punctuator(Punctuator::RightBrace))?;
            dbg!(&self.peek_token());
            Ok(ast::TypeSpecifier::Struct(StructSpecifier { identifier: name, declarations }))
        } else {
            Ok(ast::TypeSpecifier::Struct(StructSpecifier { identifier: name, declarations: vec![] }))
        }
    }
    pub fn parse_struct_declaration_list(&mut self) -> PResult<Vec<StructDeclaration>> {
        let mut struct_declarations = vec![];
        while self.can_parse_type_qualifier() || self.can_start_declaration_specifier() {
            if let Some((specifier_qualifiers, declarators)) = self.maybe_recover(
                |this| {
                    let specifier_qualifiers = this.parse_specifier_qualifier_list()?;
                    let declarators = this.parse_struct_declarator_lsit()?;
                    this.expect_token(Token::Punctuator(Punctuator::Semicolon))?;

                    Ok(Some((specifier_qualifiers, declarators)))
                },
                || None,
                Token::Punctuator(Punctuator::Semicolon),
            ) {
                struct_declarations.push(StructDeclaration { declarators, specifier_qualifiers });
            } else {
                dbg!(&self.peek_token());
                // panic!("recovered but its still blown out");
            }
        }

        Ok(struct_declarations)
    }
    pub fn parse_specifier_qualifier_list(&mut self) -> PResult<Vec<SpecifierQualifier>> {
        let mut specifier_qualifiers = vec![];
        while self.can_parse_type_qualifier() || self.can_parse_type_specifier() {
            specifier_qualifiers.push(match self.next_token()? {
                span!(Token::Keyword(Keyword::Void)) => SpecifierQualifier::Specifier(TypeSpecifier::Void),
                span!(Token::Keyword(Keyword::Char)) => SpecifierQualifier::Specifier(TypeSpecifier::Char),
                span!(Token::Keyword(Keyword::Short)) => SpecifierQualifier::Specifier(TypeSpecifier::Short),
                span!(Token::Keyword(Keyword::Int)) => SpecifierQualifier::Specifier(TypeSpecifier::Int),
                span!(Token::Keyword(Keyword::Long)) => SpecifierQualifier::Specifier(TypeSpecifier::Long),
                span!(Token::Keyword(Keyword::Float)) => SpecifierQualifier::Specifier(TypeSpecifier::Float),
                span!(Token::Keyword(Keyword::Double)) => SpecifierQualifier::Specifier(TypeSpecifier::Double),
                span!(Token::Keyword(Keyword::Signed)) => SpecifierQualifier::Specifier(TypeSpecifier::Signed),
                span!(Token::Keyword(Keyword::Unsigned)) => SpecifierQualifier::Specifier(TypeSpecifier::Unsigned),
                span!(Token::Keyword(Keyword::Bool)) => SpecifierQualifier::Specifier(TypeSpecifier::Bool),
                span!(Token::Keyword(Keyword::Complex)) => SpecifierQualifier::Specifier(TypeSpecifier::Complex),
                span!(Token::Keyword(Keyword::Struct)) => SpecifierQualifier::Specifier(self.parse_struct_type_specifier()?),
                span!(Token::Identifier(identifier)) if self.is_typedef(&identifier) => SpecifierQualifier::Specifier(TypeSpecifier::TypedefName(identifier)),
                span!(Token::Keyword(kw @ type_qualifier!())) => SpecifierQualifier::Qualifier(kw.into()),
                _ => unreachable!(),
            });
        }
        Ok(specifier_qualifiers)
    }
    pub fn parse_struct_declarator_lsit(&mut self) -> PResult<Vec<StructDeclarator>> {
        let mut declarators = vec![];
        while self.can_start_declarator() || self.consider_comma(&declarators)? {
            if declarators.len() > 0 {
                self.expect_token(Token::Punctuator(Punctuator::Comma))?;
            }
            let declarator = self.parse_declarator()?;
            declarators.push(StructDeclarator { declarator: Some(declarator) });
        }
        Ok(declarators)
    }
    fn consider_comma<T>(&mut self, v: &Vec<T>) -> PResult<bool> {
        Ok(v.len() >= 1 && matches!(self.peek_token()?, span!(Token::Punctuator(Punctuator::Comma))))
    }

    pub fn can_start_pointer(&mut self) -> PResult<bool> {
        Ok(matches!(self.peek_token()?, span!(Token::Punctuator(Punctuator::Asterisk))))
    }
    pub fn parse_pointer(&mut self) -> PResult<Spanned<Pointer>> {
        let asterist = self.expect_token(Token::Punctuator(Punctuator::Asterisk))?;
        let type_qualifiers = self.parse_type_qualifiers()?;

        if self.can_start_pointer()? {
            let ptr = self.parse_pointer()?;
            Ok(Spanned::new(asterist.range.start..ptr.range.end, Pointer { type_qualifiers, ptr: Some(Box::new(ptr)) }))
        } else {
            Ok(Spanned::new(asterist.range.start..type_qualifiers.span_fallback(self.last_location.clone()).end, Pointer { type_qualifiers, ptr: None }))
        }
    }
    pub fn parse_type_qualifiers(&mut self) -> PResult<Vec<Spanned<TypeQualifier>>> {
        let mut type_qualifiers = vec![];

        while self.can_parse_type_qualifier() {
            type_qualifiers.push(self.parse_type_qualifier()?);
        }
        Ok(type_qualifiers)
    }
    pub fn can_parse_type_qualifier(&mut self) -> bool {
        matches!(self.peek_token(), Ok(span!(Token::Keyword(type_qualifier!()))))
    }
    pub fn can_parse_type_specifier(&mut self) -> bool {
        match self.peek_token().cloned() {
            Ok(span!(Token::Keyword(type_specifier!()))) => true,
            Ok(span!(Token::Identifier(identifier))) if self.is_typedef(&identifier) => true,
            _ => false,
        }
    }
    pub fn parse_type_qualifier(&mut self) -> PResult<Spanned<TypeQualifier>> {
        let Spanned { value, range } = self.next_token()?;

        Ok(Spanned::new(
            range,
            match value {
                Token::Keyword(Keyword::Const) => ast::TypeQualifier::Const,
                Token::Keyword(Keyword::Restrict) => ast::TypeQualifier::Restrict,
                Token::Keyword(Keyword::Volatile) => ast::TypeQualifier::Volatile,
                _ => unreachable!(),
            },
        ))
    }

    pub fn parse_parameter_list(&mut self) -> PResult<Vec<Spanned<ParameterDeclaration>>> {
        let mut parameters = vec![];

        while self.can_parse_parameter() || self.consider_comma(&parameters)? {
            if parameters.len() >= 1 {
                self.expect_token(Token::Punctuator(Punctuator::Comma))?;
            }
            parameters.push(self.parse_parameter()?)
        }
        Ok(parameters)
    }
    pub fn can_parse_parameter(&mut self) -> bool {
        return self.can_start_declaration_specifier();
    }
    pub fn parse_parameter(&mut self) -> PResult<Spanned<ParameterDeclaration>> {
        //todo: abstract declarator
        let start = self.last_location.clone();
        let specifiers = self.parse_declaration_specifiers()?;
        let declarator = self.parse_declarator()?;
        let range = specifiers.span_fallback(start).start..declarator.range.end;
        Ok(Spanned::new(range, ParameterDeclaration { specifiers, declarator }))
    }
}
