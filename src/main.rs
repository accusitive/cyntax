use std::ops::Range;

use cyntax_lexer::{
    lexer::{Punctuator, Token, Whitespace},
    prelexer::PrelexerIter,
};

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
            Token::Whitespace(whitespace) => match whitespace {
                Whitespace::Space => print!("` `"),
                Whitespace::Newline => print!("\\n"),
                Whitespace::Tab => print!("\\t"),
            },
            Token::Punctuator(punctuator) => match punctuator {
                Punctuator::LParen => print!("("),
                Punctuator::RParen => print!(")"),
            },
        }
        println!();
    }
}

fn main() {
    let source = "int test()";
    let prelexer = PrelexerIter::new(source);
    let lexer = cyntax_lexer::lexer::Lexer {
        chars: prelexer.peekable(),
        source: source,
    };
    let tokens: Vec<_> = lexer.collect();
    dbg!(&tokens);

    print_tokens(source, &tokens);
    // let tokens = i.collect::<Vec<_>>();
    // dbg!(&tokens);
    // for (span, token) in tokens {
    //     println!(
    //         "{:4?} @ {:#?} -> {:2?} ",
    //         &source[span.clone()],
    //         span.clone(),
    //         token
    //     );
    // }
}
