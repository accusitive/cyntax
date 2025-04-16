use crate::Spanned;
use crate::ast::{Expression, Token};
use crate::{PResult, Parser};
use cyntax_common::ast::Punctuator;
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;

impl<'src> Parser<'src> {
    /// main parse expression
    pub fn parse_full_expression(&mut self) -> PResult<Expression> {
        if self.can_start_primary_expression() {
            self.parse_primary_expression()
        } else {
            Err(SimpleError(self.last_location.clone(), "expected one of `identifier`, `constant`, `string-literal`, `(`".to_string()).into_codespan_report())
        }
    }
    pub fn parse_primary_expression(&mut self) -> PResult<Expression> {
        todo!()
        // match self.next_token()? {
        //     span!(Token::Identifier(identifier)) => Ok(Expression::Identifier(identifier)),
        //     span!(Token::Constant(iconst)) => Ok(Expression::IntConstant(iconst)),
        //     span!(Token::StringLiteral(iconst)) => Ok(Expression::StringLiteral(iconst)),
        //     span!(Token::Punctuator(Punctuator::LeftParen)) => {
        //         let e = self.parse_expression()?;        
                
        //         self.expect_token(Token::Punctuator(Punctuator::RightParen), "to close expression")?;
        //     }
        //     _ => unreachable!(),
        // }
    }
    pub fn parse_expression(&mut self) -> PResult<Expression> {
        todo!();
    }
    pub fn can_start_primary_expression(&mut self) -> bool {
        match self.peek_token().cloned() {
            Ok(span!(Token::Identifier(identifier))) => !self.is_typedef(&identifier),
            Ok(span!(Token::Constant(_))) => true,
            Ok(span!(Token::StringLiteral(_))) => true,
            Ok(span!(Token::Punctuator(Punctuator::LeftParen))) => true,
            _ => false,
        }
    }
}
