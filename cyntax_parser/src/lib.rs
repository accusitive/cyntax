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

            self.expect_token(Token::Punctuator(Punctuator::RightBrace), "to close struct type specifier")?;
            dbg!(&self.peek_token());
            Ok(ast::TypeSpecifier::Struct(StructSpecifier { identifier: name, declarations }))
        } else {
            Ok(ast::TypeSpecifier::Struct(StructSpecifier { identifier: name, declarations: vec![] }))
        }
    }
    pub fn parse_struct_declaration_list(&mut self) -> PResult<Vec<StructDeclaration>> {
        let mut struct_declarations = vec![];
        // because i use recoverable parsing on the inside of this, its fine to not check the current token
        while !matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::RightBrace)))) {
            if let Some((specifier_qualifiers, declarators)) = self.maybe_recover(
                |this| {
                    let specifier_qualifiers = this.parse_specifier_qualifier_list()?;
                    dbg!(&specifier_qualifiers);
                    if !specifier_qualifiers.iter().any(|sq| matches!(sq, SpecifierQualifier::Specifier(_))) {
                        return Err(SimpleError(this.last_location.clone(), "Struct declarations must have atleast one specifier".to_string()).into_codespan_report());
                    }
                    let declarators = this.parse_struct_declarator_list()?;
                    this.expect_token(Token::Punctuator(Punctuator::Semicolon), "to end a struct declaration")?;

                    Ok(Some((specifier_qualifiers, declarators)))
                },
                |_| None,
                Token::Punctuator(Punctuator::Semicolon),
            ) {
                struct_declarations.push(StructDeclaration { declarators, specifier_qualifiers });
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
    pub fn parse_struct_declarator_list(&mut self) -> PResult<Vec<StructDeclarator>> {
        let mut declarators = vec![];
        while self.can_start_declarator() || self.consider_comma(&declarators)? {
            if declarators.len() > 0 {
                self.expect_token(Token::Punctuator(Punctuator::Comma), "to seperate declarators in struct declaration")?;
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
        let asterisk = self.expect_token(Token::Punctuator(Punctuator::Asterisk), "for pointer")?;
        let type_qualifiers = self.parse_type_qualifiers()?;

        if self.can_start_pointer()? {
            let ptr = self.parse_pointer()?;
            Ok(Spanned::new(asterisk.location.until(&ptr.location), Pointer { type_qualifiers, ptr: Some(Box::new(ptr)) }))
        } else {
            let range = asterisk.location.as_fallback_for_vec(&type_qualifiers);
            Ok(Spanned::new(range, Pointer { type_qualifiers, ptr: None }))
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
        let Spanned { value, location } = self.next_token()?;

        Ok(Spanned::new(
            location,
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
                self.expect_token(Token::Punctuator(Punctuator::Comma), "to seperate parameters")?;
            }
            parameters.push(self.parse_parameter(false)?)
        }
        Ok(parameters)
    }
    pub fn parse_parameter_type_list(&mut self) -> PResult<Vec<Spanned<ParameterDeclaration>>> {
        let mut parameters = vec![];

        while self.can_parse_parameter() || self.consider_comma(&parameters)? {
            if parameters.len() >= 1 {
                self.expect_token(Token::Punctuator(Punctuator::Comma), "to seperate parameters")?;
            }
            parameters.push(self.parse_parameter(true)?)
        }
        Ok(parameters)
    }
    pub fn can_parse_parameter(&mut self) -> bool {
        return self.can_start_declaration_specifier();
    }
    pub fn parse_parameter(&mut self, allow_abstract: bool) -> PResult<Spanned<ParameterDeclaration>> {
        //todo: abstract declarator
        let start = self.last_location.clone();
        let specifiers = self.parse_declaration_specifiers()?;
        let declarator = if self.can_start_declarator() { Some(self.parse_declarator()?) } else { None };
        if declarator.is_none() && !allow_abstract {
            return Err(SimpleError(start, "this declaration does not allow abstract declarators".to_string()).into_codespan_report());
        }

        let range = start.as_fallback_for_vec(&specifiers);
        Ok(Spanned::new(range, ParameterDeclaration { specifiers, declarator: declarator }))
    }
}
