use std::ops::Range;

use cyntax_lexer::{prelexer::PrelexerIter, spanned::Spanned, Punctuator, Token, Whitespace};

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
            Token::Directive(directive)  => {
                print!("#");
                match directive {
                    cyntax_lexer::Directive::DefineObject(macro_name, replacment_list) => {
                        print!("define ");
                        for range in &macro_name.1 {
                            print!("{}", &source[range.clone()]);
                        }
                        print!(" ");
                        print_tokens(source, &replacment_list);

                    },
                    cyntax_lexer::Directive::DefineFunction(macro_name, parameter_list, replacment_list) => {
                        print!("define ");
                        for range in &macro_name.1 {
                            print!("{}", &source[range.clone()]);
                        }
                        print!(" ");
                        print!("(");
                        print_tokens(source, &parameter_list.value);
                        print!(")");
                        print!(" ");
                        print_tokens(source, &replacment_list);

                    },
                    
                    cyntax_lexer::Directive::Undefine(macro_name) => {
                        print!("undef ");
                        for range in &macro_name.1 {
                            print!("{}", &source[range.clone()]);
                        }
                    },
                }
                print!("\n");
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
            Token::Whitespace(whitespace) => match whitespace {
                Whitespace::Space => print!(" "),
                Whitespace::Newline => print!("\n"),
                Whitespace::Tab => print!("\t"),
            },
            Token::Punctuator(punctuator) => print!("{}", punctuator.to_string()),
           
        }
        // println!();
    }
}

fn main() {
    let source =include_str!("../test.c");
    let lexer = cyntax_lexer::lexer::Lexer::new(source);
    let tokens: Vec<_> = lexer.collect();
    dbg!(&tokens);

    print_tokens(source, &tokens);
}
