use std::{collections::HashMap, str::FromStr};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use colored::{ColoredString, Colorize};
use cyntax_common::{
    ast::{Keyword, PreprocessingToken, Whitespace},
    ctx::{Context, File, string_interner::StringInterner},
    spanned::Spanned,
};

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
fn main() {
    let source = include_str!("../test.c");
    let mut files = HashMap::new();
    files.insert(
        0,
        File {
            name: "test.c".to_owned(),
            source: source.to_owned(),
        },
    );
    let mut ctx = Context {
        files,
        strings: StringInterner::new(),
        current_file: 0,
    };

    let lexer = cyntax_lexer::lexer::Lexer::new(&mut ctx, source);
    let tokens: Vec<_> = lexer.collect();
    dbg!(&tokens);
    print_tokens(&ctx, source, tokens.iter());
    println!();
    let pp = cyntax_preprocessor::Preprocessor::new(&mut ctx, "test.c", source, &tokens);
    let expanded = pp.expand();
    {
        println!("========================================================");
        // dbg!(&expanded);
        print_tokens(&mut ctx, source, expanded.iter());
        println!();
        // debug_spans(source, &expanded);
    }

    let mut parser = cyntax_parser::Parser::new(&mut ctx, expanded);
    let tu = parser.parse_translation_unit();

    match tu {
        Ok(tu) => {
            dbg!(&tu);
            if parser.diagnostics.len() > 0 {
                for diag in &parser.diagnostics {
                    println!("recovered: {}", diag.clone().with("test.c", source));
                }
                println!("compilation failed!");
            }
        }
        Err(e) => {
            print!("{}", e.with("test.c", source));
        }
    }
}
