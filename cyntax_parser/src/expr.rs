use crate::Spanned;
use crate::ast::{Expression, InfixOperator, Operator, PostfixOperator, PrefixOperator, Token, TypeName};
use crate::{PResult, Parser};
use cyntax_common::ast::{Keyword, Punctuator};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_lexer::span;

impl<'src> Parser<'src> {
    fn prefix_binding_power(operator: &PrefixOperator) -> ((), u8) {
        match operator {
            PrefixOperator::Plus | PrefixOperator::Minus => ((), 5),
            PrefixOperator::LogicalNot => todo!(),
            PrefixOperator::Invert => todo!(),
            PrefixOperator::SizeOf => todo!(),
            PrefixOperator::Cast => todo!(),
        }
    }
    fn infix_binding_power(operator: &InfixOperator) -> Option<(u8, u8)> {
        let bp = match operator {
            InfixOperator::Add | InfixOperator::Subtract => (1, 2),
            InfixOperator::Multiply | InfixOperator::Divide => (3, 4),
            _ => todo!(),
        };

        Some(bp)
    }
    fn postfix_binding_power(operator: &PostfixOperator) -> (u8, ()) {
        match operator {
            PostfixOperator::Increment | PostfixOperator::Decrement => (7, ()),
            _ => unreachable!(),
        }
    }

    pub fn as_infix_operator(token: &Spanned<Token>) -> Option<InfixOperator> {
        match token {
            span!(Token::Punctuator(Punctuator::Plus)) => Some(InfixOperator::Add),
            span!(Token::Punctuator(Punctuator::Asterisk)) => Some(InfixOperator::Multiply),
            _ => None,
        }
    }
    pub fn as_prefix_operator(token: &Spanned<Token>) -> Option<PrefixOperator> {
        match token {
            span!(Token::Punctuator(Punctuator::Minus)) => Some(PrefixOperator::Minus),
            span!(Token::Punctuator(Punctuator::Plus)) => Some(PrefixOperator::Plus),
            _ => None,
        }
    }
    pub fn as_postfix_operator(token: &Spanned<Token>) -> Option<PostfixOperator> {
        match token {
            span!(Token::Punctuator(Punctuator::Increment)) => Some(PostfixOperator::Increment),
            span!(Token::Punctuator(Punctuator::Decrement)) => Some(PostfixOperator::Decrement),
            _ => None,
        }
    }
    pub fn parse_full_expression(&mut self) -> PResult<Expression> {
        self.parse_expression_bp(0)
    }
    pub fn parse_expression_bp(&mut self, minimum_binding_power: u8) -> PResult<Expression> {
        let is_prefix_operator = Self::as_prefix_operator(self.peek_token()?);

        // if let guards would be cool here
        let mut lhs = if let Some(prefix_operator) = is_prefix_operator {
            let ((), right_binding_power) = Self::prefix_binding_power(&prefix_operator);
            self.next_token()?; // bump prefix operator
            let expression = self.parse_expression_bp(right_binding_power)?;
            Expression::UnaryOp(prefix_operator, Box::new(expression))
        } else {
            match self.next_token()? {
                span!(Token::Identifier(identifier)) => Expression::Identifier(identifier),
                span!(Token::Constant(iconst)) => Expression::IntConstant(iconst),
                span!(Token::StringLiteral(iconst)) => Expression::StringLiteral(iconst),
                span!(Token::Punctuator(Punctuator::LeftParen)) if !self.can_start_typename() => {
                    let expr = self.parse_expression_bp(0)?;
                    self.expect_token(Token::Punctuator(Punctuator::RightParen), "to close expression")?;
                    Expression::Parenthesized(Box::new(expr))
                }
                // Prefix operators
                // span!(Token::Punctuator(Punctuator::LeftParen)) => {
                //     let ((), right_binding_power) = Self::prefix_binding_power(PrefixOperator::Cast);
                //     let typename = self.parse_typename()?;
                //     self.expect_token(Token::Punctuator(Punctuator::RightParen), "to close cast expression")
                // }
                s => unreachable!("{:#?}", s),
            }
        };
        while !self.next_is_semicolon() {
            let peeked = self.peek_token()?;
            let post_fix_operator = Self::as_postfix_operator(peeked);

            if let Some(post_fix_operator) = post_fix_operator {
                let (left_binding_power, ()) = Self::postfix_binding_power(&post_fix_operator);
                if left_binding_power < minimum_binding_power {
                    break;
                }
                self.next_token()?; // bump past postfix operator

                lhs = Expression::PostfixOp(post_fix_operator, Box::new(lhs));
                continue;
            }

            if let Some(infix_operator) = Self::as_infix_operator(peeked) {
                let (left_binding_power, right_binding_power) = Self::infix_binding_power(&infix_operator).unwrap();

                if left_binding_power < minimum_binding_power {
                    break;
                }
                self.next_token()?; // bump infix operator

                let rhs = self.parse_expression_bp(right_binding_power)?;
                lhs = Expression::BinOp(infix_operator, Box::new(lhs), Box::new(rhs));
                continue;
            }
            break;
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

    pub fn can_start_primary_expression(&mut self) -> bool {
        match self.peek_token().cloned() {
            Ok(span!(Token::Identifier(identifier))) => !self.is_typedef(&identifier),
            Ok(span!(Token::Constant(_))) => true,
            Ok(span!(Token::StringLiteral(_))) => true,
            Ok(span!(Token::Punctuator(Punctuator::LeftParen))) => true,
            Ok(t) if Self::as_prefix_operator(&t).is_some() => true,
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
        matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::Semicolon))))
    }
}
