use cyntax_common::ast::{Keyword, Punctuator, Token};
use cyntax_common::spanned::Spanned;
use cyntax_lexer::span;

use crate::ast::{BlockItem, Statement};
use crate::{PResult, Parser};

impl Parser {
    pub fn parse_statement(&mut self) -> PResult<Statement> {
        if self.can_start_labelled_stmt() {
        } else if self.can_start_compound_statement() {
            return Ok(self.parse_compound_statement()?);
        }
        // expression stmt
        else if self.can_start_selection_statement() {
        } else if self.can_start_iteration_statement() {
        } else if self.can_start_jump_statement() {
        }
        unimplemented!("{:#?}", self.peek_token());
    }
    pub fn can_start_labelled_stmt(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Identifier(_) | Token::Keyword(Keyword::Case | Keyword::Default))));
    }
    pub fn can_start_compound_statement(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Punctuator(Punctuator::LeftBrace))));
    }
    pub fn parse_compound_statement(&mut self) -> PResult<Statement> {
        self.expect_token(Token::Punctuator(Punctuator::LeftBrace))?;
        let mut block_items = vec![];
        while self.can_start_block_item() {
            let block_item = self.parse_block_item()?;
            self.expect_token(Token::Punctuator(Punctuator::Semicolon))?;
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
        self.can_start_labelled_stmt() || self.can_start_compound_statement() || self.can_start_selection_statement() || self.can_start_iteration_statement() || self.can_start_jump_statement()
    }
    pub fn parse_block_item(&mut self) -> PResult<BlockItem> {
        if self.can_start_declaration_specifier() {
            let decl = self.parse_declaration()?;
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
    pub fn can_start_iteration_statement(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Keyword(Keyword::While | Keyword::Do | Keyword::For))));
    }
    pub fn can_start_jump_statement(&mut self) -> bool {
        return matches!(self.peek_token(), Ok(span!(Token::Keyword(Keyword::Goto | Keyword::Continue | Keyword::Break | Keyword::Return))));
    }
}
