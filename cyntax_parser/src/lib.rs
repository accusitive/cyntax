use std::{ops::Range, str::FromStr};

use ast::{DeclarationSpecifier, Declarator, ExternalDeclaration, TranslationUnit};
use cyntax_common::{
    ast::{Keyword, Punctuator, Token},
    spanned::Spanned,
};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;
use peekmore::{PeekMore, PeekMoreIterator};
#[macro_use]
pub mod patterns;
pub mod ast;
pub type PResult<T> = Result<T, cyntax_errors::why::Report>;

#[derive(Debug)]
struct TokenStream {
    iter: std::vec::IntoIter<Spanned<Token>>,
}
impl Iterator for TokenStream {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next()? {
            span!(span, Token::Identifier(identifier)) if Keyword::from_str(&identifier).is_ok() => Some(Spanned::new(span, Token::Keyword(Keyword::from_str(&identifier).unwrap()))),
            span!(span, Token::BlueIdentifier(identifier)) => Some(Spanned::new(span, Token::Identifier(identifier.to_string()))),
            span!(Token::Whitespace(_)) => self.next(),
            spanned_token => Some(spanned_token),
        }
    }
}
#[derive(Debug)]
pub struct Parser {
    token_stream: peekmore::PeekMoreIterator<TokenStream>,
    delimited_stack: Vec<PeekMoreIterator<TokenStream>>,
    last_location: Range<usize>,
}
impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Parser {
            token_stream: TokenStream { iter: tokens.into_iter() }.peekmore(),
            delimited_stack: vec![],
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
    pub fn parse_translation_unit(&mut self) -> PResult<TranslationUnit> {
        let mut external_declarations = vec![];
        while let Some(external_declaration) = self.parse_external_declaration()? {
            external_declarations.push(external_declaration);
        }
        Ok(TranslationUnit { external_declarations })
    }
    pub fn parse_external_declaration(&mut self) -> PResult<Option<ExternalDeclaration>> {
        if self.token_stream.peek().is_none() {
            return Ok(None);
        }
        let declaration = self.parse_declaration()?;

        if self.eat_if_next(Token::Punctuator(Punctuator::Semicolon))? {
        } else {
            let init_declarators = self.parse_init_declarators();
        }
        todo!();
    }

    pub fn parse_declaration(&mut self) -> PResult<()> {
        let specifiers = self.parse_declaration_specifiers()?;

        dbg!(&specifiers);
        Ok(())
    }
    pub fn parse_declaration_specifiers(&mut self) -> PResult<Vec<DeclarationSpecifier>> {
        let mut declaration_specifiers = vec![];
        while self.can_parse_declaration_specifier() {
            let specifier = self.parse_declaration_specifier()?;
            declaration_specifiers.push(specifier);
        }
        Ok(declaration_specifiers)
    }
    pub fn can_parse_declaration_specifier(&mut self) -> bool {
        // todo: add typename/identifiers to this
        return matches!(self.token_stream.peek(), Some(span!(Token::Keyword(storage_class!() | type_specifier!() | type_qualifier!() | Keyword::Inline))));
    }
    pub fn parse_declaration_specifier(&mut self) -> PResult<DeclarationSpecifier> {
        Ok(match self.next_token()? {
            // Storage-class specifiers
            span!(Token::Keyword(Keyword::Typedef)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Typedef),
            span!(Token::Keyword(Keyword::Extern)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Extern),
            span!(Token::Keyword(Keyword::Static)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Static),
            span!(Token::Keyword(Keyword::Auto)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Auto),
            span!(Token::Keyword(Keyword::Register)) => DeclarationSpecifier::StorageClass(ast::StorageClassSpecifier::Register),

            // Type specifiers
            span!(Token::Keyword(Keyword::Void)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Void),
            span!(Token::Keyword(Keyword::Char)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Char),
            span!(Token::Keyword(Keyword::Short)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Short),
            span!(Token::Keyword(Keyword::Int)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Int),
            span!(Token::Keyword(Keyword::Long)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Long),
            span!(Token::Keyword(Keyword::Float)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Float),
            span!(Token::Keyword(Keyword::Double)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Double),
            span!(Token::Keyword(Keyword::Signed)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Signed),
            span!(Token::Keyword(Keyword::Unsigned)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Unsigned),
            span!(Token::Keyword(Keyword::Bool)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Bool),
            span!(Token::Keyword(Keyword::Complex)) => DeclarationSpecifier::TypeSpecifier(ast::TypeSpecifier::Complex),

            // Type qualifiers
            span!(Token::Keyword(Keyword::Const)) => DeclarationSpecifier::TypeQualifier(ast::TypeQualifier::Const),
            span!(Token::Keyword(Keyword::Restrict)) => DeclarationSpecifier::TypeQualifier(ast::TypeQualifier::Restrict),
            span!(Token::Keyword(Keyword::Volatile)) => DeclarationSpecifier::TypeQualifier(ast::TypeQualifier::Volatile),

            // Function specifiers
            // Some(span!(Token::Keyword(Keyword::Inline))) => DeclarationSpecifier::Function(ast::FunctionSpecifier::Inline),
            x => unimplemented!("{:#?}", x),
        })
    }
    pub fn parse_init_declarators(&mut self) -> PResult<()> {
        let mut init_declarators = vec![];
        while self.can_parse_init_declarator() {
            init_declarators.push(self.parse_init_declarator()?)
        }
        Ok(())
    }

    pub fn can_parse_init_declarator(&mut self) -> bool {
        self.can_parse_declarator()
    }
    pub fn can_parse_declarator(&mut self) -> bool {
        return matches!(self.token_stream.peek(), Some(span!(Token::Punctuator(Punctuator::Asterisk) | Token::Identifier(_) | Token::Punctuator(Punctuator::LeftParen))));
    }
    pub fn parse_init_declarator(&mut self) -> PResult<()> {
        let declarator = self.parse_declarator();
        if self.eat_if_next(Token::Punctuator(Punctuator::Equal))? {
            // let initializer = self.parse_initializer();
            todo!()
        } else {
        }
        Ok(())
    }
    pub fn parse_declarator(&mut self) -> PResult<Declarator> {
        if self.eat_if_next(Token::Punctuator(Punctuator::Asterisk))? {
            let declarator = self.parse_direct_declarator()?;
            Ok(Declarator::Pointer(Box::new(declarator)))
        } else {
            panic!();
        }
    }
    pub fn parse_direct_declarator(&mut self) -> PResult<Declarator> {
        match self.peek_token()? {
            span!(Token::Identifier(identifier)) => Ok(Declarator::Identifier(identifier.clone())),
            span!(Token::Delimited {
                opener: span!('('),
                closer: span!(')'),
                inner_tokens
            }) => {
                let owned_tokens = inner_tokens.to_vec();
                Ok(Declarator::Parenthesized(Box::new(self.in_delimited(owned_tokens, |this| this.parse_declarator())?)))
            }
            x => unreachable!("{:#?}", x),
        }
    }
    pub fn in_delimited<T, F: FnMut(&mut Self) -> T>(&mut self, tokens: Vec<Spanned<Token>>, mut f: F) -> T {
        self.delimited_stack.push(TokenStream { iter: tokens.into_iter() }.peekmore());
        let result = f(self);
        self.delimited_stack.pop();
        result
    }
}
