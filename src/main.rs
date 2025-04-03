use cyntax_common::{
    ast::{Token, Whitespace},
    spanned::Spanned,
};

#[cfg(test)]
mod tests;
fn print_tokens<'src, I: Iterator<Item = &'src Spanned<Token>>>(source: &'src str, tokens: I) {
    for spanned_token in tokens {
        match &spanned_token.value {
            Token::Identifier(ranges) => {
                for range in ranges.iter() {
                    print!("{}", &source[range.clone()]);
                }
            }
            Token::StringLiteral(ranges) => {
                print!("\"");
                for range in ranges {
                    print!("{}", &source[range.clone()]);
                }
                print!("\"");
            }
            Token::PPNumber(ranges) => {
                for range in ranges.iter() {
                    print!("{}", &source[range.clone()]);
                }
            }
            Token::Delimited {
                opener: opening,
                closer: Some(closing),
                inner_tokens: tokens,
            } => {
                print!("{}", opening);
                print_tokens(source, tokens.value.iter());
                print!("{}", closing);
            }
            Token::Delimited {
                opener: opening,
                closer: None,
                inner_tokens: tokens,
            } => {
                print!("{}", opening);
                print_tokens(source, tokens.value.iter());
            }
            Token::ControlLine(inner) => {
                print!("#");
                print_tokens(source, inner.iter());
            }
            Token::Whitespace(whitespace) => match whitespace {
                Whitespace::Space => print!(" "),
                Whitespace::Newline => print!("\n"),
                Whitespace::Tab => print!("\t"),
            },
            Token::Punctuator(punctuator) => print!("{}", punctuator.to_string()),
        }
    }
}

fn main() {
    let source = include_str!("../test.c");
    let lexer = cyntax_lexer::lexer::Lexer::new("test.c", source);
    let tokens: Vec<_> = lexer.collect();
    // dbg!(&tokens);
    print_tokens(source, tokens.iter());
    println!();
    let mut pp = cyntax_preprocessor::Preprocessor::new("test.c", source, &tokens);
    {
        let expanded = pp.expand();
        println!("========================================================");
        dbg!(&expanded);
        print_tokens(source, expanded.iter());
        println!();
    }
}
