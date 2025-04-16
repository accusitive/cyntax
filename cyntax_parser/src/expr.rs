use crate::Spanned;
use crate::ast::{BinaryOperator, Expression, Token, TypeName};
use crate::{PResult, Parser};
use cyntax_common::ast::{Keyword, Punctuator};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;

impl<'src> Parser<'src> {
    /// main parse expression
    pub fn parse_full_expression(&mut self) -> PResult<Expression> {
        if self.can_start_primary_expression() {
            self.parse_expression()
        } else {
            Err(SimpleError(self.last_location.clone(), "expected one of `identifier`, `constant`, `string-literal`, `(`".to_string()).into_codespan_report())
        }
    }
    pub fn parse_primary_expression(&mut self) -> PResult<Expression> {
        match self.next_token()? {
            span!(Token::Identifier(identifier)) => Ok(Expression::Identifier(identifier)),
            span!(Token::Constant(iconst)) => Ok(Expression::IntConstant(iconst)),
            span!(Token::StringLiteral(iconst)) => Ok(Expression::StringLiteral(iconst)),
            span!(Token::Punctuator(Punctuator::LeftParen)) if !self.can_start_typename() => {
                let expr = self.parse_expression()?;
                self.expect_token(Token::Punctuator(Punctuator::RightParen), "to close expression")?;
                Ok(Expression::Parenthesized(Box::new(expr)))
            }
            s => unreachable!("{:#?}", s),
        }
    }
    pub fn parse_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_assignment_expression()?;
        Ok(expr)
    }
    pub fn parse_assignment_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_conditional_expression()?;
        Ok(expr)
    }
    pub fn parse_conditional_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_logical_or_expression()?;
        Ok(expr)
    }
    pub fn parse_logical_or_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_logical_and_expression()?;
        Ok(expr)
    }
    pub fn parse_logical_and_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_inclusive_or_expression()?;
        Ok(expr)
    }
    pub fn parse_inclusive_or_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_exclusive_or_expression()?;
        Ok(expr)
    }
    pub fn parse_exclusive_or_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_and_expression()?;
        Ok(expr)
    }
    pub fn parse_and_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_equality_expression()?;
        Ok(expr)
    }
    pub fn parse_equality_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_relational_expression()?;
        Ok(expr)
    }
    pub fn parse_relational_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_shift_expression()?;
        Ok(expr)
    }
    pub fn parse_shift_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_additive_expression()?;
        Ok(expr)
    }
    pub fn parse_additive_expression(&mut self) -> PResult<Expression> {
        let mut lhs = self.parse_multiplicative_expression()?;
        loop {
            if self.eat_if_next(Token::Punctuator(Punctuator::Plus))? {
                let rhs = self.parse_multiplicative_expression()?;
                lhs = Expression::BinOp(BinaryOperator::Add, Box::new(lhs), Box::new(rhs));
            } else if self.eat_if_next(Token::Punctuator(Punctuator::Minus))? {
                let rhs = self.parse_multiplicative_expression()?;
                lhs = Expression::BinOp(BinaryOperator::Subtract, Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }
    pub fn parse_multiplicative_expression(&mut self) -> PResult<Expression> {
        let mut lhs = self.parse_cast_expression()?;
        loop {
            if self.next_is_semicolon() {
                break;
            }
            if self.eat_if_next(Token::Punctuator(Punctuator::Asterisk))? {
                let rhs = self.parse_cast_expression()?;
                lhs = Expression::BinOp(BinaryOperator::Mul, Box::new(lhs), Box::new(rhs));
            } else if self.eat_if_next(Token::Punctuator(Punctuator::Slash))? {
                let rhs = self.parse_cast_expression()?;
                lhs = Expression::BinOp(BinaryOperator::Div, Box::new(lhs), Box::new(rhs));
            } else if self.eat_if_next(Token::Punctuator(Punctuator::Percent))? {
                let rhs = self.parse_cast_expression()?;
                lhs = Expression::BinOp(BinaryOperator::Mod, Box::new(lhs), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(lhs)
    }
    pub fn can_start_typename(&mut self) -> bool {
        match self.peek_token().cloned() {
            Ok(span!(Token::Keyword(type_specifier!() | type_qualifier!()))) => true,
            Ok(span!(Token::Identifier(identifier))) if self.is_typedef(&identifier) => true,
            _ => false,
        }
    }
    pub fn parse_cast_expression(&mut self) -> PResult<Expression> {
        let mut expr = None;
        loop {
            if self.next_is_semicolon() {
                break;
            }
            if self.eat_if_next(Token::Punctuator(Punctuator::LeftParen))? {
                let typename = self.parse_typename()?;
                self.expect_token(Token::Punctuator(Punctuator::RightParen), "to close cast expression")?;
                expr = Some(Expression::Cast(typename, Box::new(self.parse_unary_expression()?)));
            } else {
                break;
            }
        }
        Ok(expr.unwrap_or(self.parse_unary_expression()?))
    }
    pub fn parse_unary_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_postfix_expression()?;
        Ok(expr)
    }
    pub fn parse_postfix_expression(&mut self) -> PResult<Expression> {
        let expr = self.parse_primary_expression()?;
        Ok(expr)
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
    pub fn parse_typename(&mut self) -> PResult<TypeName> {
        let sq = self.parse_specifier_qualifier_list()?;
        assert!(sq.len() > 0);
        let d = self.parse_abstract_declarator()?;

        Ok(TypeName { specifier_qualifiers: sq, declarator: d })
    }
    pub fn next_is_semicolon(&mut self) -> bool {
        match self.peek_matches(Token::Punctuator(Punctuator::Semicolon)) {
            Ok(next_is_semi) => next_is_semi,
            Err(_) => true
        }
    }
}
