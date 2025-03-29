use std::ops::Range;

use cyntax_lexer::{prelexer::PrelexerIter, Punctuator, Token, Whitespace};

#[cfg(test)]
mod tests;
fn print_tokens(source: &str, tokens: &[(Range<usize>, Token)]) {
    for (_range, token) in tokens {
        match token {
            Token::Identifier(ranges) => {
                for range in ranges {
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
            Token::Whitespace(whitespace) => match whitespace {
                Whitespace::Space => print!("` `"),
                Whitespace::Newline => print!("\\n"),
                Whitespace::Tab => print!("\\t"),
            },
            Token::Punctuator(punctuator) => print!("{}", punctuator.to_string()),
           
        }
        println!();
    }
}

fn main() {
    let source =include_str!("../test.c");
    let lexer = cyntax_lexer::lexer::Lexer::new(source);
    let tokens: Vec<_> = lexer.collect();
    dbg!(&tokens);

    print_tokens(source, &tokens);
}
