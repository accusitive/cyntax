use cyntax_common::ast::{Keyword, Punctuator};
use cyntax_common::spanned::Spanned;
use cyntax_errors::Diagnostic;
use cyntax_errors::errors::SimpleError;
use cyntax_lexer::span;

use crate::ast::{BlockItem, Statement, Token};
use crate::{PResult, Parser};

impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> PResult<Statement> {
        if self.can_start_labelled_stmt() {
        } else if self.can_start_compound_statement() {
            let compound_stmt = self.parse_compound_statement()?;
            return Ok(compound_stmt);
        } else if self.can_start_primary_expression() {
            let expr = self.parse_expression()?;
            self.expect_token(Token::Punctuator(Punctuator::Semicolon), "after expression")?;
            return Ok(Statement::Expression(expr));
        } else if self.can_start_selection_statement() {
            return Ok(self.parse_selection_statement()?);
        } else if self.can_start_iteration_statement() {
        } else if self.can_start_jump_statement() {
            return self.parse_jump_statement();
        }
        Err(SimpleError(self.last_location.clone(), format!(" failed to parse statement starting with {:?}", self.peek_token())).into_codespan_report())
        // unimplemented!("{:#?}", self.peek_token());
    }
    pub fn can_start_labelled_stmt(&mut self) -> bool {
        let a = matches!(self.peek_token(), Ok(span!(Token::Identifier(_)))) && matches!(self.peek_token_nth(1), Ok(span!(Token::Punctuator(Punctuator::Colon))));
        return a || matches!(self.peek_token(), Ok(span!(Token::Keyword(Keyword::Case | Keyword::Default))));
    }
    pub fn can_start_compound_statement(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::LeftBrace))));
    }
    pub fn parse_compound_statement(&mut self) -> PResult<Statement> {
        self.expect_token(Token::Punctuator(Punctuator::LeftBrace), "to open compound statement")?;
        let mut block_items = vec![];
        while self.can_start_block_item() {
            let block_item = self.parse_block_item()?;
            block_items.push(block_item);
        }
        if self.eat_if_next(Token::Punctuator(Punctuator::RightBrace))? {
            Ok(Statement::Compound(block_items))
        } else {
            todo!("{:#?}", self.peek_token());
        }
    }
    pub fn can_start_block_item(&mut self) -> bool {
        self.can_start_declaration_specifier() || self.can_start_statement()
    }
    pub fn can_start_statement(&mut self) -> bool {
        self.can_start_labelled_stmt() || self.can_start_compound_statement() || self.can_start_selection_statement() || self.can_start_iteration_statement() || self.can_start_jump_statement() || self.can_start_primary_expression()
    }
    pub fn parse_block_item(&mut self) -> PResult<BlockItem> {
        if self.can_start_declaration_specifier() {
            let decl = self.parse_declaration()?;
            self.expect_token(Token::Punctuator(Punctuator::Semicolon), "after each declaration")?;
            Ok(BlockItem::Declaration(decl))
        } else if self.can_start_statement() {
            let stmt = self.parse_statement()?;
            Ok(BlockItem::Statement(stmt))
        } else {
            todo!()
        }
    }
    pub fn can_start_selection_statement(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Keyword(Keyword::If | Keyword::Switch))));
    }
    pub fn parse_selection_statement(&mut self) -> PResult<Statement> {
        if self.eat_if_next(Token::Keyword(Keyword::If))? {
            self.expect_token(Token::Punctuator(Punctuator::LeftParen), "to open if statement's condition expression")?;
            let expr = self.parse_expression()?;
            self.expect_token(Token::Punctuator(Punctuator::RightParen), "to close if statement's condition expression")?;
            let then = self.parse_statement()?;
            if self.eat_if_next(Token::Keyword(Keyword::Else))? {
                let elze = self.parse_statement()?;
                return Ok(Statement::If(expr, Box::new(then), Some(Box::new(elze))));
            } else {
                return Ok(Statement::If(expr, Box::new(then), None));
            }
        } else if matches!(self.peek_token(), Ok(span!(Token::Keyword(Keyword::Goto)))) {
            unimplemented!()
        } else {
            todo!()
        }
    }
    pub fn can_start_iteration_statement(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Keyword(Keyword::While | Keyword::Do | Keyword::For))));
    }
    pub fn can_start_jump_statement(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Keyword(Keyword::Goto | Keyword::Continue | Keyword::Break | Keyword::Return))));
    }
    pub fn parse_jump_statement(&mut self) -> PResult<Statement> {
        let jump_stmt = match self.next_token()? {
            span!(Token::Keyword(Keyword::Goto)) => {
                let identifier = self.expect_identifier()?;
                self.expect_token(Token::Punctuator(Punctuator::Semicolon), "after goto statement")?;
                Statement::Goto(identifier)
            }
            span!(Token::Keyword(Keyword::Continue)) => Statement::Continue,
            span!(Token::Keyword(Keyword::Break)) => Statement::Break,
            span!(Token::Keyword(Keyword::Return)) => {
                if matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::Semicolon)))) {
                    Statement::Return(None)
                } else if self.can_start_primary_expression() {
                    let e = self.parse_expression()?;

                    // dbg!("returning", &e);
                    Statement::Return(Some(e))
                } else {
                    return Err(SimpleError(self.last_location.clone(), "cannot start primary expression".to_string()).into_codespan_report());
                }
            }
            _ => unreachable!(),
        };
        self.expect_token(Token::Punctuator(Punctuator::Semicolon), "after jump statement   ")?;

        Ok(jump_stmt)
    }
}
