use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{lexer::Punctuator, preprocess::ast::Token};

use super::{ast::*, ParseResult, Parser};
impl<'a> Parser<'a> {
    pub fn parse_compound_statement(&mut self) -> ParseResult<Statement> {
        self.expect_token(&Token::Punctuator(Punctuator::LBrace))?;

        self.push_scope();

        let mut statements = vec![];

        while !self.consume_if_present(&Token::Punctuator(Punctuator::RBrace))?.value {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        self.pop_scope();

        Ok(Statement::Block(statements))
    }
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        let next = self.peek_token()?;
        if matches!(next.value, Token::Punctuator(Punctuator::LBrace)) {
            Ok(self.parse_compound_statement()?)
        } else if matches!(next.value, Token::Keyword(Keyword::If)) {
            self.expect_token(&Token::Keyword(Keyword::If))?;

            let lp = self.expect_token(&Token::Punctuator(Punctuator::LParen))?.shell();
            let cond = match self.parse_expression()? {
                Some(e) => e,
                None => {
                    return Err(Diagnostic::error().with_message("If statement with no condition expression").with_labels(vec![
                        Label::primary(lp.file_id(), lp.location_range()).with_message("Expected expression after this parenthesis")
                    ]))
                }
            };
            self.expect_token(&Token::Punctuator(Punctuator::RParen))?;
            let then = self.parse_statement()?;

            if self.consume_if_present(&Token::Keyword(Keyword::Else))?.value {
                let elze = self.parse_statement()?;
                Ok(Statement::IfElse(cond, Box::new(then), Box::new(elze)))
            } else {
                Ok(Statement::If(cond, Box::new(then)))
            }
        } else if matches!(next.value, Token::Keyword(Keyword::Switch)) {
            unimplemented!()
        } else if matches!(next.value, Token::Keyword(Keyword::While) | Token::Keyword(Keyword::Do)) {
            unimplemented!()
        } else if matches!(
            next.value,
            Token::Keyword(Keyword::Goto) | Token::Keyword(Keyword::Continue) | Token::Keyword(Keyword::Break)
        ) {
            unimplemented!()
        } else if matches!(next.value, Token::Keyword(Keyword::For)) {
            self.expect_token(&Token::Keyword(Keyword::For))?;
            self.expect_token(&Token::Punctuator(Punctuator::LParen))?;
            let init = if !self.consume_if_present(&Token::Punctuator(Punctuator::Semicolon))?.value {
                let init = match self.parse_expression()? {
                    Some(e) => ForInitializor::Expression(e),
                    None => {
                        let d = self.parse_declaration(false)?;
                        self.expect_token(&Token::Punctuator(Punctuator::Semicolon))?;
                        ForInitializor::Declaration(d)
                    }
                };

                Some(init)
            } else {
                None
            };
            let cond = self.parse_expression()?;
            self.expect_token(&Token::Punctuator(Punctuator::Semicolon))?;
            let update = self.parse_expression()?;

            self.expect_token(&Token::Punctuator(Punctuator::RParen))?;
            let body = self.parse_statement()?;

            Ok(Statement::For(init, cond, update, Box::new(body)))
        } else if matches!(next.value, Token::Keyword(Keyword::Return)) {
            self.expect_token(&Token::Keyword(Keyword::Return))?;
            let expr = self.parse_expression()?.unwrap();
            self.expect_token(&Token::Punctuator(Punctuator::Semicolon))?;
            Ok(Statement::Return(expr))
        } else if let Token::Identifier(i) = &next.value {
            // The identifier is a typedef, and thus probably a declaration
            if self.resolve_typedef2(i) {
                let s = self.parse_declaration(false)?;
                self.expect_token(&Token::Punctuator(Punctuator::Semicolon))?;

                Ok(Statement::Declaration(s))

            // The identifier is a variable, and thus an expression
            } else if self.resolve_identifier2(i) {
                let e = self.parse_expression()?;
                self.expect_token(&Token::Punctuator(Punctuator::Semicolon))?;
                Ok(Statement::Expression(e.unwrap()))
            } else {
                Err(Diagnostic::error()
                    .with_message("Not a valid type name")
                    .with_labels(vec![Label::primary(next.file_id(), next.location_range())
                        .with_message(format!("`{}` is not defined as a type, or an identifier", next.value.as_identifier().unwrap()))]))
            }
        } else {
            let s = self.parse_declaration(false)?;
            self.expect_token(&Token::Punctuator(Punctuator::Semicolon))?;

            Ok(Statement::Declaration(s))
        }
    }
}
