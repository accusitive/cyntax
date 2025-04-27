use std::{collections::HashMap, fmt::Display};

use cyntax_common::ctx::ParseContext;
use cyntax_hir::{self as hir, HirId};
use cyntax_mir::{self as mir, BasicBlock, Instruction, InstructionKind, Operand, StackSlotId, Value};

pub type PResult<T> = Result<T, cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>;

pub struct HirLower<'src> {
    pctx: &'src ParseContext,
}

impl<'src> HirLower<'src> {
    pub fn new(ctx: &'src ParseContext) -> Self {
        Self { pctx: ctx }
    }
    pub fn lower(&mut self, tu: &hir::TranslationUnit) -> PResult<Vec<mir::Function>> {
        let mut funcs = vec![];
        for declaration in tu.declarations {
            match declaration {
                cyntax_hir::ExternalDeclaration::FunctionDefinition(function_definition) => {
                    funcs.push(self.lower_function_definition(function_definition)?);
                }

                cyntax_hir::ExternalDeclaration::Declaration(declaration) => todo!(),
            }
        }
        Ok(funcs)
    }
    pub fn lower_function_definition(&mut self, funcdef: &hir::FunctionDefinition) -> PResult<mir::Function> {
        let mut func = mir::Function {
            name: (),
            slots: vec![],
            blocks: vec![BasicBlock { instructions: vec![] }],
        };
        FunctionLowerer::new(&mut func).lower(funcdef);

        Ok(func)
    }
}

pub struct FunctionLowerer<'a> {
    func: &'a mut mir::Function,
    map: HashMap<HirId, StackSlotId>,
    next_id: usize,
}

impl<'a, 'hir> FunctionLowerer<'a> {
    pub fn new(func: &'a mut mir::Function) -> Self {
        Self { func, map: HashMap::new(), next_id: 0 }
    }
    fn allocate_stack_slot(&mut self, size: usize) -> StackSlotId {
        let id = self.func.slots.len();
        self.func.slots.push(size);
        StackSlotId(id)
    }
    pub fn lower(&mut self, funcdef: &hir::FunctionDefinition<'hir>) {
        self.lower_statement(funcdef.body);
    }
    pub fn lower_statement(&mut self, stmt: &hir::Statement<'hir>) {
        match stmt.kind {
            cyntax_hir::StatementKind::Compound(block_items) => {
                for block_item in block_items {
                    match block_item {
                        cyntax_hir::BlockItem::Declaration(declaration) => {
                            let slot = self.allocate_stack_slot(4);
                            self.map.insert(declaration.id, slot.clone());

                            if let Some(init) = declaration.init {
                                match init {
                                    cyntax_hir::Initializer::Assignment(expression) => {
                                        let val = self.lower_expression(&expression);

                                        self.stack_store(slot, val);
                                    }
                                }
                            }
                        }
                        cyntax_hir::BlockItem::Statement(statement) => {
                            self.lower_statement(statement);
                        }
                    }
                }
            }
            cyntax_hir::StatementKind::Expression(expression) => todo!(),
            cyntax_hir::StatementKind::Return(expression) => {}
            cyntax_hir::StatementKind::While(expression, statement) => todo!(),
            cyntax_hir::StatementKind::Continue => todo!(),
            cyntax_hir::StatementKind::Break => todo!(),
        }
    }
    pub fn lower_expression(&mut self, expr: &hir::Expression<'hir>) -> Operand {
        match &expr.kind {
            cyntax_hir::ExpressionKind::Constant(spanned) => {
                let num = spanned.value.number.parse::<i64>().unwrap();
                Operand::Constant(num)
            }
            cyntax_hir::ExpressionKind::BinaryOp(spanned, lhs, rhs) => {
                let lhs = self.lower_expression(&lhs);
                let rhs = self.lower_expression(&rhs);
                Operand::Value(self.add(lhs, rhs))
            },
            cyntax_hir::ExpressionKind::DeclarationReference(hir_id) => {
                Operand::Place(self.map.get(hir_id).unwrap().clone())
            }
            cyntax_hir::ExpressionKind::Cast(ty, expression) => todo!(),
            cyntax_hir::ExpressionKind::MemberAccess(expression, spanned) => todo!(),
        }
    }
    fn next_value_id(&mut self) -> usize {
        let next_id = self.next_id;

        self.next_id += 1;
        next_id
    }
    pub fn stack_store(&mut self, slot: StackSlotId, value: Operand) {
        self.insert(mir::InstructionKind::Store, vec![Operand::Place(slot), value], None);
    }
    pub fn const_i64(&mut self, value: i64) -> Value {
        self.insert(cyntax_mir::InstructionKind::Const, vec![mir::Operand::Constant(value)], Some(mir::Ty::I64)).unwrap()
    }
    pub fn add(&mut self, lhs: Operand, rhs: Operand) -> Value {
        self.insert(InstructionKind::Add, vec![lhs, rhs], Some(mir::Ty::I64)).unwrap()
    }
    pub fn insert(&mut self, kind: InstructionKind, inputs: Vec<Operand>, output: Option<mir::Ty>) -> Option<mir::Value> {
        let output = output.map(|ty| {
            let id = self.next_value_id();
            mir::Value { id, ty }
        });
        let ins = Instruction { inputs, output: output.clone(), kind };
        self.func.blocks.last_mut().unwrap().instructions.push(ins);

        output
    }
}

