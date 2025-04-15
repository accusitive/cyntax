use std::{collections::HashMap, fmt::Debug};

use cyntax_common::{
    ast::{PreprocessingToken, Punctuator},
    ctx::{
        Context,
        string_interner::{backend::StringBackend, symbol::SymbolU32},
    },
    spanned::Spanned,
};
use cyntax_lexer::{lexer::Lexer, span};

use crate::{expand::MacroArgument, prepend::PrependingPeekableIterator};

pub struct ArgumentSubstitutionIterator<'a, I>
where
    I: Debug + Iterator<Item = Spanned<PreprocessingToken>>,
{
    pub ctx: &'a mut Context,
    pub replacements: PrependingPeekableIterator<I>,
    pub map: HashMap<SymbolU32, MacroArgument>,
    pub is_variadic: bool,
    pub variadic_args: Vec<MacroArgument>,
    pub glue_next_token: bool,

    pub glue_string: String,

    pub stringify_next_token: bool,
    pub stringify_string: String,
}

impl<'a, I: Debug + Iterator<Item = Spanned<PreprocessingToken>>> Iterator for ArgumentSubstitutionIterator<'a, I> {
    type Item = Vec<Spanned<PreprocessingToken>>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.replacements.next();
        dbg!(&token);
        let token = token?;

        match &token {
            token if self.glue_next_token => {
                let a = self.maybe_substitute_arg(token.clone(), false);
                Self::stringify_tokens(&self.ctx.strings, a.iter(), &mut self.glue_string);

                if !matches!(self.replacements.peek(), Some(span!(PreprocessingToken::Punctuator(Punctuator::HashHash)))) {
                    let src = format!("{}", self.glue_string);

                    let tokens = Lexer::new(self.ctx, &src).map(|span| Spanned::new(token.range.clone(), span.value)).collect::<Vec<_>>();

                    self.glue_next_token = false;
                    self.glue_string.clear();
                    Some(tokens)
                } else {
                    self.replacements.next().unwrap();
                    Some(vec![])
                }
            }
            token if self.stringify_next_token => {
                dbg!(&token);
                let a = self.maybe_substitute_arg(token.clone(), false);
                Self::stringify_tokens(&self.ctx.strings, a.iter(), &mut self.stringify_string);
                self.replacements.prepend(Spanned::new(token.range.clone(), PreprocessingToken::StringLiteral(self.ctx.strings.get_or_intern(&self.stringify_string))));
                self.stringify_next_token = false;
                self.stringify_string.clear();
                Some(vec![])
            }

            span!(PreprocessingToken::Punctuator(Punctuator::Hash)) => {
                self.stringify_next_token = true;
                Some(vec![])
            }
            token if matches!(self.replacements.peek(), Some(span!(PreprocessingToken::Punctuator(Punctuator::HashHash)))) => {
                self.glue_next_token = true;
                let a = self.maybe_substitute_arg(token.clone(), false);
                Self::stringify_tokens(&self.ctx.strings, a.iter(), &mut self.glue_string);
                self.replacements.next().unwrap(); // // eat ## 
                Some(vec![])
            }
            span!(PreprocessingToken::Identifier(identifier)) if self.map.contains_key(identifier) => {
                let expanded = self.map.get(identifier).unwrap().expanded.clone();
                Some(expanded)
            }
            span!(PreprocessingToken::Identifier(identifier)) if self.is_variadic && *identifier == self.ctx.strings.get_or_intern_static("__VA_ARGS__") => Some(
                self.variadic_args
                    .iter()
                    .map(|arg| arg.expanded.clone())
                    .flatten()
                    .intersperse(Spanned::new(0..0, PreprocessingToken::Punctuator(Punctuator::Comma)))
                    .collect::<Vec<_>>(),
            ),
            _ => Some(vec![token]),
        }
    }
}

impl<'a, I: Debug + Iterator<Item = Spanned<PreprocessingToken>>> ArgumentSubstitutionIterator<'a, I> {
    pub fn maybe_substitute_arg(&mut self, token: Spanned<PreprocessingToken>, expand: bool) -> Vec<Spanned<PreprocessingToken>> {
        match token {
            span!(PreprocessingToken::Identifier(identifier)) if self.map.contains_key(&identifier) => {
                let expanded = if expand { self.map.get(&identifier).unwrap().expanded.clone() } else { self.map.get(&identifier).unwrap().unexpanded.clone() };
                expanded
            }
            _ => vec![token],
        }
    }
    pub fn stringify_tokens<'b, T: Iterator<Item = &'b Spanned<PreprocessingToken>>>(strings: &cyntax_common::ctx::string_interner::StringInterner<StringBackend>, tokens: T, s: &mut String) {
        for token in tokens {
            Self::stringify_token(strings, token, s);
        }
    }
    pub fn stringify_token(strings: &cyntax_common::ctx::string_interner::StringInterner<StringBackend>, token: &Spanned<PreprocessingToken>, s: &mut String) {
        match &token.value {
            PreprocessingToken::Identifier(identifier) => s.push_str(strings.resolve(*identifier).unwrap()),
            PreprocessingToken::BlueIdentifier(identifier) => s.push_str(strings.resolve(*identifier).unwrap()),

            PreprocessingToken::StringLiteral(string) => {
                s.push('"');
                s.push_str(strings.resolve(*string).unwrap());
                s.push('"');
            }
            PreprocessingToken::CharLiteral(chars) => {
                s.push('\'');
                s.push_str(strings.resolve(*chars).unwrap());
                s.push('\'');
            }
            PreprocessingToken::PPNumber(number) => {
                s.push_str(strings.resolve(*number).unwrap());
            }
            PreprocessingToken::Whitespace(whitespace) => {
                s.push(match whitespace {
                    cyntax_common::ast::Whitespace::Space => ' ',
                    cyntax_common::ast::Whitespace::Newline => '\n',
                    cyntax_common::ast::Whitespace::Tab => '\t',
                });
            }
            PreprocessingToken::Punctuator(punctuator) => s.push_str(&punctuator.to_string()),
            PreprocessingToken::Delimited(d) => {
                s.push(d.opener.value);
                Self::stringify_tokens(strings, d.inner_tokens.iter(), s);
                s.push(d.closer.value);
            }
            _ => unreachable!(),
        }
    }
}
