use std::{collections::HashMap, str::FromStr};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::{SimpleFile, SimpleFiles},
};
use colored::{ColoredString, Colorize};
use cyntax_common::{
    ast::{Keyword, PreprocessingToken, Whitespace},
    ctx::{Context, File, HasContext, string_interner::StringInterner},
    spanned::Spanned,
};
use cyntax_errors::UnwrapDiagnostic;

#[cfg(test)]
mod tests;
fn p(s: ColoredString) {
    print!("{}", s);
}
fn print_tokens<'src, I: Iterator<Item = &'src Spanned<PreprocessingToken>>>(ctx: &'src Context, source: &'src str, tokens: I) {
    for spanned_token in tokens {
        match &spanned_token.value {
            PreprocessingToken::Identifier(identifier) if Keyword::from_str(ctx.strings.resolve(*identifier).unwrap()).is_ok() => {
                p(format!("{}", ctx.strings.resolve(*identifier).unwrap()).green());
            }
            PreprocessingToken::Identifier(identifier) => {
                p(format!("{}", ctx.strings.resolve(*identifier).unwrap()).white());
            }
            PreprocessingToken::BlueIdentifier(identifier) => {
                p(format!("{}", ctx.strings.resolve(*identifier).unwrap()).blue());
            }
            PreprocessingToken::StringLiteral(string) => {
                p(format!("\"").green());
                p(format!("{}", ctx.strings.resolve(*string).unwrap()).green());
                p(format!("\"").green());
            }
            PreprocessingToken::CharLiteral(chars) => {
                p(format!("\'").bright_green());
                p(format!("{}", ctx.strings.resolve(*chars).unwrap()).bright_green());
                p(format!("\'").bright_green());
            }
            PreprocessingToken::PPNumber(number) => {
                p(format!("{}", ctx.strings.resolve(*number).unwrap()).bright_blue());
            }
            PreprocessingToken::Delimited(d) => {
                p(format!("{}", d.opener.value).on_black());
                print_tokens(ctx, source, d.inner_tokens.iter());
                p(format!("{}", d.closer.value).on_black());
            }
            PreprocessingToken::ControlLine(inner) => {
                p(format!("#").purple());
                print_tokens(ctx, source, inner.iter());
            }
            PreprocessingToken::Whitespace(whitespace) => match whitespace {
                Whitespace::Space => print!(" "),
                Whitespace::Newline => print!("\n"),
                Whitespace::Tab => print!("\t"),
            },
            PreprocessingToken::Punctuator(punctuator) => p(format!("{}", punctuator.to_string()).purple()),
        }
    }
}
// fn debug_spans(source: &str, tokens: &[Spanned<PreprocessingToken>]) {
//     for token in tokens {
//         let diag = Diagnostic::new(codespan_reporting::diagnostic::Severity::Note).with_label(Label {
//             file_id: 0,
//             message: format!("debug: token {:?}", token),
//             range: token.range.clone(),
//             style: codespan_reporting::diagnostic::LabelStyle::Primary,
//         });

//         println!("{}", cyntax_errors::write_codespan_report(diag, "test.c", source));
//     }
// }
struct WithContext<'src> {
    context: &'src mut Context,
}
impl<'src> WithContext<'src> {
    pub fn with_context<F: FnOnce(Self)>(self, f: F) {
        f(self)
    }
}
impl<'src> HasContext for WithContext<'src> {
    fn ctx(&self) -> &Context {
        &self.context
    }
}
fn main() {
    let mut files = SimpleFiles::new();
    let source = include_str!("../test.c");
    let file = files.add("test.c".to_owned(), source.to_owned());

    let mut ctx = Context {
        files,
        strings: StringInterner::new(),
        current_file: file,
    };

    let lexer = cyntax_lexer::lexer::Lexer::new(&mut ctx, source);
    let tokens: Vec<_> = lexer.collect();

    {
        dbg!(&tokens);
        print_tokens(&ctx, source, tokens.iter());
        println!();
    }

    let pre_processor = cyntax_preprocessor::Preprocessor::new(&mut ctx, "test.c", source, &tokens);
    let pre_processor_result = pre_processor.expand();

    let expanded = WithContext { context: &mut ctx }.unwrap_diagnostic(pre_processor_result);
    {
        println!("========================================================");
        print_tokens(&mut ctx, source, expanded.iter());
        println!("========================================================");
    }

    let mut parser = cyntax_parser::Parser::new(&mut ctx, expanded);
    let parse_result = parser.parse_translation_unit();
    let tu = WithContext { context: &mut ctx }.unwrap_diagnostic(parse_result);
    dbg!(&tu);
}
