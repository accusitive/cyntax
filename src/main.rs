use codespan_reporting::diagnostic::{Diagnostic, Label};
use cyntax_common::{
    ast::{Keyword, PreprocessingToken, Whitespace},
    spanned::Spanned,
};

#[cfg(test)]
mod tests;
fn print_tokens<'src, I: Iterator<Item = &'src Spanned<PreprocessingToken>>>(source: &'src str, tokens: I) {
    for spanned_token in tokens {
        match &spanned_token.value {
            PreprocessingToken::Identifier(identifier) => {
                print!("{}", identifier);
            }
            PreprocessingToken::BlueIdentifier(identifier) => {
                print!("{}", identifier);
            }
            PreprocessingToken::StringLiteral(string) => {
                print!("\"");
                print!("{}", string);
                print!("\"");
            }
            PreprocessingToken::CharLiteral(chars) => {
                print!("\'");
                print!("{}", chars);
                print!("\'");
            }
            PreprocessingToken::PPNumber(number) => {
                print!("{}", number);
            }
            PreprocessingToken::Delimited {
                opener: opening,
                closer: closing,
                inner_tokens: tokens,
            } => {
                print!("{}", opening.value);
                print_tokens(source, tokens.iter());
                print!("{}", closing.value);
            }
            PreprocessingToken::ControlLine(inner) => {
                print!("#");
                print_tokens(source, inner.iter());
            }
            PreprocessingToken::Whitespace(whitespace) => match whitespace {
                Whitespace::Space => print!(" "),
                Whitespace::Newline => print!("\n"),
                Whitespace::Tab => print!("\t"),
            },
            PreprocessingToken::Punctuator(punctuator) => print!("{}", punctuator.to_string()),
        }
    }
}
fn debug_spans(source: &str, tokens: &[Spanned<PreprocessingToken>]) {
    for token in tokens {
        let diag = Diagnostic::new(codespan_reporting::diagnostic::Severity::Note).with_label(Label {
            file_id: 0,
            message: format!("debug: token {:?}", token),
            range: token.range.clone(),
            style: codespan_reporting::diagnostic::LabelStyle::Primary,
        });

        println!("{}", cyntax_errors::write_codespan_report(diag, "test.c", source));
    }
}
fn main() {
    let source = include_str!("../test.c");
    let lexer = cyntax_lexer::lexer::Lexer::new("test.c", source);
    let tokens: Vec<_> = lexer.collect();
    dbg!(&tokens);
    print_tokens(source, tokens.iter());
    println!();
    let pp = cyntax_preprocessor::Preprocessor::new("test.c", source, &tokens);
    let expanded = pp.expand();
    {
        println!("========================================================");
        // dbg!(&expanded);
        print_tokens(source, expanded.iter());
        println!();
        // debug_spans(source, &expanded);
    }

    let mut parser = cyntax_parser::Parser::new(expanded);
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
