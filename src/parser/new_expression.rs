/// TODO: use this instead of the old one/
/// its closer to the original c spec but i dont feel like doing it right now
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{lexer::Punctuator, preprocess::Token};

use super::{ast::*, ParseResult, Parser, L};
impl<'a> Parser<'a> {
    pub fn expr(&mut self) -> ParseResult<Option<L<Expression>>> {
        Ok(self.parse_primary_expression()?)
    }
    pub fn parse_primary_expression(&mut self) -> ParseResult<Option<L<Expression>>> {
        let next = self.peek_token()?;
        let e = match &next.value {
            Token::Identifier(identifier) => Some(next.same(Expression::Identifier(identifier.to_string()))),
            Token::StringLiteral(string) => Some(next.same(Expression::StringLiteral(string.to_string()))),
            Token::Number(number) => Some(next.same(Expression::Number(number.to_string()))),
            Token::Punctuator(Punctuator::LParen) => {
                let e = self.parse_expression()?.unwrap();

                self.expect_token(&Token::Punctuator(Punctuator::RParen))?;

                Some(e)
            }
            _ => todo!()
        };

        Ok(e)
    }
    pub fn parse_expression(&mut self) -> ParseResult<L<Expression>> {

        let cond_expr = self.parse_conditional_expression();
        
        // let mut assignment_expressions = vec![];
        // while let Some(assign_expr) = self.parse_assignment_expression()? {
        //     assignment_expressions.push(assign_expr);
        //     if assignment_expressions.len() > 0 {
        //         self.expect_token(&Token::Punctuator(Punctuator::Comma))?;
        //     }
        // }
        // assignment_expressions
    }
    pub fn parse_binary_operator(&mut self) -> ParseResult<L<BinaryOperation>> {
        let next = self.next_token()?;
        Ok(next.same(match next.value {
            Token::Punctuator(Punctuator::Plus) => Ok(BinaryOperation::Add),
            Token::Punctuator(Punctuator::Minus) => Ok(BinaryOperation::Sub),
            Token::Punctuator(Punctuator::Asterisk) => Ok(BinaryOperation::Mul),
            Token::Punctuator(Punctuator::Slash) => Ok(BinaryOperation::Div),
            Token::Punctuator(Punctuator::Percent) => Ok(BinaryOperation::Mod),

            Token::Punctuator(Punctuator::LeftAngle) => Ok(BinaryOperation::Less),
            Token::Punctuator(Punctuator::RightAngle) => Ok(BinaryOperation::Greater),

            Token::Punctuator(Punctuator::LessOrEqual) => Ok(BinaryOperation::LessOrEqual),
            Token::Punctuator(Punctuator::GreatorOrEqual) => Ok(BinaryOperation::GreatorOrEqual),

            Token::Punctuator(Punctuator::Equal) => Ok(BinaryOperation::Assign),

            Token::Punctuator(Punctuator::PlusEqual) => Ok(BinaryOperation::AssignBySum),
            Token::Punctuator(Punctuator::MinusEqual) => Ok(BinaryOperation::AssignByDifference),
            Token::Punctuator(Punctuator::AsteriskEqual) => Ok(BinaryOperation::AssignByProduct),
            Token::Punctuator(Punctuator::SlashEqual) => Ok(BinaryOperation::AssignByQuotient),
            Token::Punctuator(Punctuator::PercentEqual) => Ok(BinaryOperation::AssignByRemainder),

            _ => Err(Diagnostic::error()
                .with_message("invalid binary operator")
                .with_labels(vec![Label::primary(next.file_id(), next.location_range())])),
        }?))
    }
    pub fn parse_unary_operator(&mut self) -> ParseResult<L<UnaryOperator>> {
        let next = self.next_token()?;
        Ok(next.same(match next.value {
            Token::Punctuator(Punctuator::PlusPlus) => Ok(UnaryOperator::Increment),

            _ => Err(Diagnostic::error()
                .with_message("invalid unary operator")
                .with_labels(next.generate_location_labels())),
        }?))
    }
    fn get_token_precedence(p: &Token) -> Option<i32> {
        match p {
            Token::Punctuator(Punctuator::Asterisk) | Token::Punctuator(Punctuator::Slash) | Token::Punctuator(Punctuator::Percent) => Some(4),
            Token::Punctuator(Punctuator::Plus) | Token::Punctuator(Punctuator::Minus) => Some(3),
            Token::Punctuator(Punctuator::LeftAngle)
            | Token::Punctuator(Punctuator::RightAngle)
            | Token::Punctuator(Punctuator::LessOrEqual)
            | Token::Punctuator(Punctuator::GreatorOrEqual) => Some(2),

            Token::Punctuator(Punctuator::Equal)
            | Token::Punctuator(Punctuator::PlusEqual)
            | Token::Punctuator(Punctuator::MinusEqual)
            | Token::Punctuator(Punctuator::AsteriskEqual)
            | Token::Punctuator(Punctuator::SlashEqual)
            | Token::Punctuator(Punctuator::PercentEqual) => Some(1),
            _ => None,
        }
    }
}
