use crate::Spanned;
use crate::ast::{Expression, Token};
use crate::{PResult, Parser};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;

impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> PResult<Expression> {
        if self.can_start_primary_expression() {
            self.parse_primary_expression()
        } else {
            Err(SimpleError(self.last_location.clone(), "expected one of `identifier`, `constant`, `string-literal`, `(`".to_string()).into_codespan_report())
        }
    }
    pub fn parse_primary_expression(&mut self) -> PResult<Expression> {
        match self.next_token()? {
            span!(Token::Identifier(identifier)) => Ok(Expression::Identifier(identifier)),

            _ => unreachable!(),
        }
    }
    pub fn can_start_primary_expression(&mut self) -> bool {
        match self.peek_token().cloned() {
            Ok(span!(Token::Identifier(identifier))) => !self.is_typedef(&identifier),
            // Ok(span!(Token::PPNumber(number))) => true,
            _ => false,
        }
    }
}
