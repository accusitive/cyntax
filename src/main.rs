use cyntax_lexer::{Token, Whitespace, spanned::Spanned};

#[cfg(test)]
mod tests;
fn print_tokens(source: &str, tokens: &[Spanned<Token>]) {
    for spanned_token in tokens {
        match &spanned_token.value {
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
            Token::PPNumber(ranges) => {
                for range in ranges {
                    print!("{}", &source[range.clone()]);
                }
            }
            Token::Delimited(opening, closing, tokens) => {
                print!("{}", opening);
                print_tokens(source, &tokens.value);
                print!("{}", closing);
            }
            Token::ControlLine(inner) => {
                print!("#");
                print_tokens(source, &inner);
            }
            Token::Whitespace(whitespace) => match whitespace {
                Whitespace::Space => print!(" "),
                Whitespace::Newline => print!("\n"),
                Whitespace::Tab => print!("\t"),
            },
            Token::Punctuator(punctuator) => print!("{}", punctuator.to_string()),
        }
    }
    println!();
}

fn main() {
    let source = include_str!("../test.c");
    let lexer = cyntax_lexer::lexer::Lexer::new("test.c", source);
    let tokens: Vec<_> = lexer.collect();
    // dbg!(&tokens);
    print_tokens(source, &tokens);

    let mut pp = cyntax_preprocessor::Preprocessor::new("test.c", source, &tokens);
    pp.create_token_tree();
}