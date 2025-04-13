use crate::ast::*;
use cyntax_common::{
    ast::*,
    spanned::{Span, Spanned},
};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;
use peekmore::{PeekMore, PeekMoreIterator};
use std::{ops::Range, str::FromStr};

#[macro_use]
pub mod patterns;
pub mod ast;
pub mod decl;
pub mod stmt;
pub type PResult<T> = Result<T, cyntax_errors::why::Report>;

#[derive(Debug)]
struct TokenStream {
    iter: std::vec::IntoIter<Spanned<Token>>,
}
impl Iterator for TokenStream {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next()? {
                span!(span, Token::Identifier(identifier)) if Keyword::from_str(&identifier).is_ok() => return Some(Spanned::new(span, Token::Keyword(Keyword::from_str(&identifier).unwrap()))),
                span!(span, Token::BlueIdentifier(identifier)) => return Some(Spanned::new(span, Token::Identifier(identifier.to_string()))),
                span!(Token::Whitespace(_)) => continue,
                spanned_token => return Some(spanned_token),
            }
        }
    }
}
#[derive(Debug)]
pub struct Parser {
    token_stream: peekmore::PeekMoreIterator<TokenStream>,
    last_location: Range<usize>,
}
impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Parser {
            token_stream: TokenStream { iter: tokens.into_iter() }.peekmore(),
            last_location: 0..0,
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
    pub fn expect_identifier(&mut self) -> PResult<Spanned<String>> {
        match self.next_token()? {
            span!(span, Token::Identifier(identifier)) => Ok(Spanned::new(span, identifier)),
            stoken => Err(SimpleError(stoken.range, format!("expected identifier, found {:?}", stoken.value)).into_why_report()),
        }
    }
    pub fn parse_translation_unit(&mut self) -> PResult<TranslationUnit> {
        let mut external_declarations = vec![];
        while let Some(external_declaration) = self.parse_external_declaration()? {
            external_declarations.push(external_declaration);
        }
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
            Ok(ast::TypeSpecifier::Struct(StructSpecifier { identifier: name, declarations }))
        } else {
            Ok(ast::TypeSpecifier::Struct(StructSpecifier { identifier: name, declarations: vec![] }))
        }
    }
    pub fn parse_struct_declaration_list(&mut self) -> PResult<StructDeclaration> {
        // let specifier_qualifier = self.parse_Speci
        todo!()
    }
    pub fn parse_specifier_qualifier_list(&mut self) -> PResult<SpecifierQualifier> {
        let mut specifier_qualifiers = vec![];
        while self.can_parse_type_qualifier() || self.can_parse_type_specifier() {
            match self.next_token()? {
                span!(kw @ Token::Keyword(type_specifier!())) => {
                    specifier_qualifiers.push(TypeSpecifier::from(kw));

                },
                span!(kw @ Token::Keyword(type_qualifier!())) => {
                    self.parse_declaration_specifier()
                    // specifier_qualifiers.push(TypeQualifier::from(kw));

                },
                _ => unreachable!()
            }
        }
        Ok(specifier_qualifiers)
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
        matches!(self.peek_token(), Ok(span!(Token::Keyword(type_specifier!()))))
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
