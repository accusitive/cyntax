use std::ops::Deref;

use cyntax_common::spanned::{Location, Spanned};
use cyntax_hir::BlockItem;
use cyntax_parser::{ast::{self, IterationStatement}, constant::{self, IntConstant}, PResult};
use crate::{hir, AstLower};

impl<'src, 'hir> AstLower<'src, 'hir> {

    pub fn lower_statement(&mut self, statement: &Spanned<ast::Statement>) -> PResult<&'hir hir::Statement<'hir>> {
        #[allow(unused_variables)]
        let kind = match &statement.value {
            ast::Statement::Labeled(labeled_statement) => todo!(),
            ast::Statement::Compound(block_items) => {
                self.push_scope();
                let mut hir_block_items = vec![];
                for item in block_items {
                    match item {
                        ast::BlockItem::Declaration(declaration) => {
                            hir_block_items.extend(self.lower_declaration(&declaration)?.into_iter().map(|decl| hir::BlockItem::Declaration(decl)));
                        }
                        ast::BlockItem::Statement(statement) => {
                            hir_block_items.push(hir::BlockItem::Statement(self.lower_statement(&statement)?));
                        }
                    }
                }
                self.pop_scope();
                hir::StatementKind::Compound(self.arena.alloc_slice_fill_iter(hir_block_items.into_iter()))
            }
            ast::Statement::Expression(expression) => hir::StatementKind::Expression(self.arena.alloc(self.lower_expression(expression)?)),
            ast::Statement::Iteration(IterationStatement::While(condition, body)) => hir::StatementKind::While(self.lower_expression(condition)?, self.lower_statement(&body)?),
            ast::Statement::Iteration(IterationStatement::ForLoop { init, condition, update, body }) => {
                let mut block_items = vec![];
                if let Some(init) = init {
                    match init {
                        ast::ForInit::Expression(expression) => {
                            let id = self.next_id();
                            block_items.push(hir::BlockItem::Statement(self.arena.alloc(hir::Statement {
                                id,
                                span: expression.location.clone(),
                                kind: hir::StatementKind::Expression(self.lower_expression(expression)?),
                            })));
                        }
                        ast::ForInit::Declaration(spanned) => {
                            let bi = self.lower_declaration(&spanned)?;
                            block_items.extend(bi.into_iter().map(|declaration| BlockItem::Declaration(declaration)));
                        }
                    }
                }
                let control = if let Some(condition) = condition {
                    self.lower_expression(condition)?
                } else {
                    let id = self.next_id();
                    self.arena.alloc(hir::Expression {
                        id,
                        loc: Location::new(),
                        kind: hir::ExpressionKind::Constant(Location::new().to_spanned(IntConstant {
                            number: "1".to_string(),
                            suffix: constant::Suffix {
                                signed: constant::Signedness::None,
                                width: constant::Width::None,
                            },
                            base: 10,
                        })),
                    })
                };
                let inner = {
                    let id = self.next_id();
                    let mut block_items = vec![];
                    block_items.push(hir::BlockItem::Statement(self.lower_statement(&body)?));

                    if let Some(update) = update {
                        block_items.push(hir::BlockItem::Statement(self.arena.alloc(hir::Statement {
                            id,
                            span: update.location.clone(),
                            kind: hir::StatementKind::Expression(self.lower_expression(update)?),
                        })));
                    }
                    let block_items: &'hir _ = self.arena.alloc_slice_fill_iter(block_items.into_iter());
                    self.arena.alloc(hir::Statement {
                        id,
                        span: body.location.clone(),
                        kind: hir::StatementKind::Compound(block_items),
                    })
                };
                let id = self.next_id();
                block_items.push(BlockItem::Statement(self.arena.alloc(hir::Statement {
                    id,
                    span: statement.location.clone(),
                    kind: hir::StatementKind::While(control, inner),
                })));
                let block_items: &'hir _ = self.arena.alloc_slice_fill_iter(block_items.into_iter());
                hir::StatementKind::Compound(block_items)
            }
            ast::Statement::Iteration(iteration_statement) => todo!(),
            ast::Statement::Goto(spanned) => todo!(),
            ast::Statement::Continue => hir::StatementKind::Continue,
            ast::Statement::Break => hir::StatementKind::Break,
            ast::Statement::Return(Some(expression)) => hir::StatementKind::Return(Some(self.lower_expression(expression)?)),
            ast::Statement::Return(None) => hir::StatementKind::Return(None),
            ast::Statement::If(condition, then, statement1) => {
                let condition = self.lower_expression(condition)?;
                let then = self.lower_statement(then.deref())?;
                if let Some(elze) = statement1 {
                    hir::StatementKind::IfThenElse(condition, then, self.lower_statement(elze.deref())?)
                } else {
                    hir::StatementKind::IfThen(condition, then)
                }
            }
            ast::Statement::Switch(spanned, statement) => todo!(),
            ast::Statement::Error => panic!(),
        };

        let id = self.next_id();
        Ok(self.arena.alloc(hir::Statement { id, span: statement.location.clone(), kind }))
    }
}