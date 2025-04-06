use cyntax_common::{
    ast::{Token, Whitespace},
    spanned::Spanned,
};

#[cfg(test)]
mod tests;
fn print_tokens<'src, I: Iterator<Item = &'src Spanned<Token>>>(source: &'src str, tokens: I) {
    for spanned_token in tokens {
        match &spanned_token.value {
            Token::Identifier(identifier) => {
                print!("{}", identifier);
            }
            Token::StringLiteral(string) => {
                print!("\"");
                print!("{}", string);
                print!("\"");
            }
            Token::PPNumber(number) => {
                print!("{}", number);
            }
            Token::Delimited {
                opener: opening,
                closer: closing,
                inner_tokens: tokens,
            } => {
                print!("{}", opening.value);
                print_tokens(source, tokens.iter());
                print!("{}", closing.value);
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
