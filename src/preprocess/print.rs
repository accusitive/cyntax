use crate::{lexer::PreprocessingToken, location::LocationHistory};

use super::{ast::Token, TokenStream};

pub fn print(ts: &[LocationHistory<Token>]) {
    let mut line = 1;
    for token in ts {
        let out = match &token.value {
            Token::Keyword(keyword) => format!("{} ", keyword.as_str()),
            Token::Identifier(identifier) => format!("{} ", identifier),
            Token::Constant(constant) => format!(""),
            Token::StringLiteral(s) => format!("\"{}\"", s),
            Token::Punctuator(punctuator) => format!("{}", punctuator.stringify()),
        };
        // if token.location.start.line != line {
        //     println!();
        //     line = token.location.end.line;
        // }
        print!("{}", out);

    }
    println!();

    // panic!();
}
