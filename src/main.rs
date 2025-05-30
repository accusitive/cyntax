use std::str::FromStr;

use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles, term::termcolor::Ansi};
use colored::{ColoredString, Colorize};
// use cyntax_ast_lower::{check::TyCheckVisitor, visit::Visitor};
use cyntax_common::{
    ast::{Keyword, PreprocessingToken, Whitespace},
    ctx::{HasContext, ParseContext, string_interner::StringInterner},
    spanned::Spanned,
};
use cyntax_errors::UnwrapDiagnostic;

#[cfg(test)]
mod tests;
fn p(s: ColoredString) {
    print!("{}", s);
}
fn print_tokens<'src, I: Iterator<Item = &'src Spanned<PreprocessingToken>>>(ctx: &'src ParseContext, source: &'src str, tokens: I) {
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
                print_tokens(ctx, source, std::iter::once(&d.opener));
                print_tokens(ctx, source, d.inner_tokens.iter());
                print_tokens(ctx, source, std::iter::once(&d.closer));
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
fn print_diagnostics(ctx: &ParseContext, diagnostics: &[Diagnostic<usize>]) {
    let mut output_buffer = Vec::new();
    let config = codespan_reporting::term::Config::default();
    let mut ansi_writer = Ansi::new(&mut output_buffer);
    for diag in diagnostics {
        codespan_reporting::term::emit(&mut ansi_writer, &config, &ctx.files, diag).unwrap();
    }
    println!("{}", String::from_utf8(output_buffer).unwrap());
}

fn main() {
    // let m = cyntax_mir::test();
    // let mut ctx = ParseContext {
    //     files: SimpleFiles::new(),
    //     strings: StringInterner::new(),
    //     current_file: 0,
    // };
    // let cliff_lower = cyntax_backend::CliffLower::new(&mut ctx);

    // cliff_lower.lower(&m);
    // todo!();
    let mut files = SimpleFiles::new();
    // let source = include_str!("../test.c");
    let source = std::fs::read_to_string("./test.c").unwrap();
    let source = source.as_str();
    let file = files.add("test.c".to_owned(), source.to_owned());

    let mut ctx = ParseContext {
        files,
        strings: StringInterner::new(),
        current_file: file,
    };

    let lexer = cyntax_lexer::lexer::Lexer::new(&mut ctx, source);
    let tokens: Vec<_> = lexer.collect();

    {
        // dbg!(&tokens);
        print_tokens(&ctx, source, tokens.iter());
        println!();
    }

    let pre_processor = cyntax_preprocessor::Preprocessor::new(&mut ctx, &tokens);
    let pre_processor_result = pre_processor.expand();

    let expanded = ctx.unwrap_diagnostic(pre_processor_result);
    {
        println!("========================================================");
        print_tokens(&ctx, source, expanded.iter());
        println!("\n========================================================");
    }

    let mut parser = cyntax_parser::Parser::new(&mut ctx, expanded);
    let parse_result = parser.parse_translation_unit();

    match parse_result {
        Ok(tu) => {
            dbg!(&tu);
            let mut output_buffer = Vec::new();
            let config = codespan_reporting::term::Config::default();
            let mut ansi_writer = Ansi::new(&mut output_buffer);
            for diag in &parser.diagnostics {
                codespan_reporting::term::emit(&mut ansi_writer, &config, &ctx.files, diag).unwrap();
            }
            println!("{}", String::from_utf8(output_buffer).unwrap());

            let arena = cyntax_ast_lower::Bump::new();
            let mut lower = cyntax_ast_lower::AstLower::new(&mut ctx, &arena);
            let hir = lower.lower(&tu);
            let (hir_tu, diags, map) = WithContext { ctx: &mut ctx }.unwrap_diagnostic(hir);
            dbg!(&hir_tu);

            {
                print_diagnostics(&ctx, &diags);
            }

            let mut mir_lower = cyntax_hir_lower::HirLower::new(&mut ctx, map);
            let mir_result = mir_lower.lower(hir_tu);
            let mir = WithContext { ctx: &mut ctx }.unwrap_diagnostic(mir_result);
            dbg!(&mir);
            println!("{}", mir.functions.last().unwrap());

            let cl = cyntax_backend::CliffLower::new(&mut ctx);
            cl.lower(&mir);
        }
        Err(e) => {
            let mut output_buffer = Vec::new();
            let config = codespan_reporting::term::Config::default();
            let mut ansi_writer = Ansi::new(&mut output_buffer);
            for diag in &parser.diagnostics {
                codespan_reporting::term::emit(&mut ansi_writer, &config, &ctx.files, diag).unwrap();
            }
            // codespan_reporting::term::emit(&mut ansi_writer, &config, &ctx.files, &e).unwrap();

            println!("{}", String::from_utf8(output_buffer).unwrap());
            ctx.unwrap_diagnostic(Err::<(), _>(e));
        }
    }
}
struct WithContext<'src> {
    ctx: &'src mut ParseContext,
}
impl<'src> HasContext for WithContext<'src> {
    fn ctx(&self) -> &ParseContext {
        self.ctx
    }
}
