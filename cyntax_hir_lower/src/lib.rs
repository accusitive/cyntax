use std::{collections::HashMap, fmt::Display, ops::Deref};

use cyntax_common::ctx::ParseContext;
use cyntax_hir::{self as hir, HirId, HirMap, StructTypeKind};
use cyntax_mir::{self as mir, BasicBlock, BlockId, Instruction, InstructionKind, Operand, StackSlotId, Value};

pub type PResult<T> = Result<T, cyntax_errors::codespan_reporting::diagnostic::Diagnostic<usize>>;

pub struct HirLower<'src, 'hir> {
    pctx: &'src ParseContext,
    hir_map: HirMap<'hir>,
}

impl<'src, 'hir> HirLower<'src, 'hir> {
    pub fn new(ctx: &'src ParseContext, hir_map: HirMap<'hir>) -> Self {
        Self { pctx: ctx, hir_map }
    }
    pub fn lower(&mut self, tu: &hir::TranslationUnit) -> PResult<mir::TranslationUnit> {
        let mut funcs = vec![];
        for declaration in tu.declarations {
            match declaration {
                cyntax_hir::ExternalDeclaration::FunctionDefinition(function_definition) => {
                    funcs.push(self.lower_function_definition(function_definition)?);
                }

                cyntax_hir::ExternalDeclaration::Declaration(declaration) => todo!(),
            }
        }

        Ok(mir::TranslationUnit { data: (), functions: funcs })
    }
    pub fn lower_function_definition(&mut self, funcdef: &hir::FunctionDefinition) -> PResult<mir::Function> {
        let mut func = mir::Function {
            ty: None,
            name: funcdef.identifier,
            slots: vec![],
            blocks: vec![BasicBlock { instructions: vec![] }],
        };
        FunctionLowerer::new(&mut func, &self.hir_map).lower(funcdef);

        Ok(func)
    }
}

pub struct FunctionLowerer<'a, 'hir> {
    func: &'a mut mir::Function,
    ss_map: HashMap<HirId, StackSlotId>,
    next_id: usize,
    current_block: BlockId,
    hir_map: &'a HirMap<'hir>,
}

impl<'a, 'hir> FunctionLowerer<'a, 'hir> {
    pub fn new(func: &'a mut mir::Function, hir_map: &'a HirMap<'hir>) -> Self {
        Self {
            func,
            ss_map: HashMap::new(),
            next_id: 0,
            current_block: BlockId(0),
            hir_map,
        }
    }
    fn lower_ty_kind(&mut self, ty: &hir::TyKind) -> mir::Ty {
        match &ty {
            cyntax_hir::TyKind::Base(specifier_qualifiers) => match specifier_qualifiers.specifiers {
                cyntax_hir::TypeSpecifierStateMachine::None => panic!("Unexpected None type specifier"),
                cyntax_hir::TypeSpecifierStateMachine::Void => panic!("void?"),
                cyntax_hir::TypeSpecifierStateMachine::Char => mir::Ty::U8,
                cyntax_hir::TypeSpecifierStateMachine::SignedChar => mir::Ty::I8,
                cyntax_hir::TypeSpecifierStateMachine::UnsignedChar => mir::Ty::U8,
                cyntax_hir::TypeSpecifierStateMachine::Short => mir::Ty::I16,
                cyntax_hir::TypeSpecifierStateMachine::SignedShort => mir::Ty::I16,
                cyntax_hir::TypeSpecifierStateMachine::ShortInt => mir::Ty::I16,
                cyntax_hir::TypeSpecifierStateMachine::SignedShortInt => mir::Ty::I16,
                cyntax_hir::TypeSpecifierStateMachine::UnsignedShort => mir::Ty::U16,
                cyntax_hir::TypeSpecifierStateMachine::UnsignedShortInt => mir::Ty::U16,
                cyntax_hir::TypeSpecifierStateMachine::Int => mir::Ty::I32,
                cyntax_hir::TypeSpecifierStateMachine::Signed => mir::Ty::I32,
                cyntax_hir::TypeSpecifierStateMachine::SignedInt => mir::Ty::I32,
                cyntax_hir::TypeSpecifierStateMachine::Unsigned => mir::Ty::U32,
                cyntax_hir::TypeSpecifierStateMachine::UnsignedInt => mir::Ty::U32,
                cyntax_hir::TypeSpecifierStateMachine::Long => mir::Ty::I64,
                cyntax_hir::TypeSpecifierStateMachine::SignedLong => mir::Ty::I64,
                cyntax_hir::TypeSpecifierStateMachine::LongInt => mir::Ty::I64,
                cyntax_hir::TypeSpecifierStateMachine::SignedLongInt => mir::Ty::I64,
                cyntax_hir::TypeSpecifierStateMachine::UnsignedLong => mir::Ty::U64,
                cyntax_hir::TypeSpecifierStateMachine::UnsignedLongInt => mir::Ty::U64,
                cyntax_hir::TypeSpecifierStateMachine::LongLong => mir::Ty::I64,
                cyntax_hir::TypeSpecifierStateMachine::SignedLongLong => mir::Ty::I64,
                cyntax_hir::TypeSpecifierStateMachine::LongLongInt => mir::Ty::I64,
                cyntax_hir::TypeSpecifierStateMachine::SignedLongLongInt => mir::Ty::I64,
                cyntax_hir::TypeSpecifierStateMachine::UnsignedLongLong => mir::Ty::U64,
                cyntax_hir::TypeSpecifierStateMachine::UnsignedLongLongInt => mir::Ty::U64,
                cyntax_hir::TypeSpecifierStateMachine::Float => mir::Ty::F32,
                cyntax_hir::TypeSpecifierStateMachine::Double => mir::Ty::F64,
                cyntax_hir::TypeSpecifierStateMachine::LongDouble => mir::Ty::F64, // Often maps to f64
                cyntax_hir::TypeSpecifierStateMachine::StructOrUnion(hir_id) => {
                    let struct_ty = self.hir_map.tags.get(&hir_id).unwrap();
                    let mut struct_fields = vec![];
                    if let StructTypeKind::Complete(fields) = struct_ty.kind {
                        let mut offset = 0;
                        for field in fields {
                            let mir_ty = self.lower_ty_kind(&field.ty.kind);
                            let alignment = mir_ty.align_of();
                            let padding_needed = (alignment - (offset % alignment)) % alignment;

                            for _ in 0..padding_needed {
                                struct_fields.push(mir::StructField::Padding(mir::Ty::U8));
                            }
                            offset += padding_needed + mir_ty.size_of();
                            match field.identifier {
                                Some(identifier) => {
                                    struct_fields.push(mir::StructField::Named(identifier, mir_ty));
                                }
                                None => {
                                    struct_fields.push(mir::StructField::Anonymous(mir_ty));
                                }
                            }
                        }
                    }
                    mir::Ty::Struct(struct_fields)
                }
                cyntax_hir::TypeSpecifierStateMachine::Typedef(identifier) => todo!(),
                cyntax_hir::TypeSpecifierStateMachine::Bool => mir::Ty::U8,
                _ => todo!(),
            },
            cyntax_hir::TyKind::Pointer(spanneds, ty_kind) => {
                let inner = self.lower_ty_kind(ty_kind.deref());
                mir::Ty::Ptr(Box::new(inner))
            }
            cyntax_hir::TyKind::Function { return_ty, parameters } => todo!(),
            cyntax_hir::TyKind::Array(ty_kind, expression) => todo!(),
        }
    }
    fn allocate_stack_slot(&mut self, mir_ty: &mir::Ty) -> StackSlotId {
        let id = self.func.slots.len();
        self.func.slots.push(cyntax_mir::Slot { size: mir_ty.size_of(), ty: mir_ty.clone() });
        StackSlotId(id)
    }
    fn allocate_block(&mut self) -> BlockId {
        let id = self.func.blocks.len();
        self.func.blocks.push(BasicBlock { instructions: vec![] });
        BlockId(id)
    }
    pub fn lower(&mut self, funcdef: &hir::FunctionDefinition<'hir>) {
        if let hir::TyKind::Function { return_ty, parameters: _ } = &funcdef.ty.kind {
            let ret = self.lower_ty_kind(&return_ty);
            self.func.ty = Some(ret);
        }
        self.lower_statement(funcdef.body);
    }
    pub fn lower_statement(&mut self, stmt: &hir::Statement<'hir>) {
        match stmt.kind {
            cyntax_hir::StatementKind::Compound(block_items) => {
                for block_item in block_items {
                    match block_item {
                        cyntax_hir::BlockItem::Declaration(declaration) => {
                            let mir_ty = self.lower_ty_kind(&declaration.ty.kind);

                            let slot = self.allocate_stack_slot(&mir_ty);
                            self.ss_map.insert(declaration.id, slot.clone());

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
            cyntax_hir::StatementKind::Expression(expression) => {
                self.lower_expression(expression);
            }
            cyntax_hir::StatementKind::Return(Some(expression)) => {
                let value = self.lower_expression(expression);
                self.insert(InstructionKind::ReturnValue, vec![value], None);
            }
            cyntax_hir::StatementKind::Return(expression) => {
                self.insert(InstructionKind::Return, vec![], None);
            }
            cyntax_hir::StatementKind::While(expression, statement) => todo!(),
            cyntax_hir::StatementKind::Continue => todo!(),
            cyntax_hir::StatementKind::Break => todo!(),
            cyntax_hir::StatementKind::IfThen(expression, then_statement) => {
                let then_bb = self.allocate_block();
                let cont_bb = self.allocate_block();
                let condition = self.lower_expression(expression);

                self.insert(InstructionKind::JumpIf, vec![condition, Operand::BlockId(then_bb), Operand::BlockId(cont_bb)], None);
                self.current_block = then_bb;
                self.lower_statement(then_statement);

                // Technically this isnt needed (its handled in insert) at all, but I don't know how to feel about the behaviour of c allowing
                // int main(){
                //   return 1;
                //   return 1;
                // }

                if !self.current_block_has_terminator() {
                    self.insert(InstructionKind::Jump, vec![Operand::BlockId(cont_bb)], None);
                }
                self.current_block = cont_bb;
            }
            cyntax_hir::StatementKind::IfThenElse(expression, then_statement, elze_statement) => {
                let then_bb = self.allocate_block();
                let elze_bb = self.allocate_block();
                let cont_bb = self.allocate_block();
                let condition = self.lower_expression(expression);

                self.insert(InstructionKind::JumpIf, vec![condition, Operand::BlockId(then_bb), Operand::BlockId(elze_bb)], None);

                self.current_block = then_bb;
                self.lower_statement(then_statement);

                if !self.current_block_has_terminator() {
                    self.insert(InstructionKind::Jump, vec![Operand::BlockId(cont_bb)], None);
                }
                self.current_block = elze_bb;
                self.lower_statement(elze_statement);
                if !self.current_block_has_terminator() {
                    self.insert(InstructionKind::Jump, vec![Operand::BlockId(cont_bb)], None);
                }
                self.current_block = cont_bb;
            }
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
            }
            cyntax_hir::ExpressionKind::DeclarationReference(hir_id) => Operand::Place(self.ss_map.get(hir_id).unwrap().clone()),
            cyntax_hir::ExpressionKind::Cast(ty, expression) => todo!(),
            cyntax_hir::ExpressionKind::MemberAccess(expression, spanned) => todo!(),
            cyntax_hir::ExpressionKind::AddressOf(expression) => {
                let operand = self.lower_expression(&expression);
                match operand {
                    // Operand::Value(ref value) => {
                    //     let stack_slot = self.allocate_stack_slot(&value.ty);
                    //     self.stack_store(stack_slot, operand.clone());
                    //     let addr = self.insert(InstructionKind::StackAddr, vec![Operand::Place(stack_slot)], Some(cyntax_mir::Ty::Ptr(Box::new(value.ty.clone())))).unwrap();
                    //     Operand::Value(addr)
                    // }

                    Operand::Place(stack_slot) => {
                        let ty = self.func.slots[stack_slot.0].ty.clone();
                        let addr = self.insert(InstructionKind::StackAddr, vec![operand.clone()], Some(cyntax_mir::Ty::Ptr(Box::new(ty)))).unwrap();
                        Operand::Value(addr)
                    }
                    _ => todo!(),
                }
            }
            cyntax_hir::ExpressionKind::Dereference(expression) => {
                let o = self.lower_expression(&expression);
                match &o {
                    Operand::Value(value) => {
                        let deref_ty = match &value.ty {
                            cyntax_mir::Ty::Ptr(ty) => ty,
                            t => unreachable!("dereferencing non pointer {:?}", t),
                        };
                        Operand::Value(self.insert(InstructionKind::Load, vec![o.clone()], Some(*deref_ty.clone())).unwrap())
                    }
                    Operand::Constant(_) => todo!(),
                    Operand::Place(stack_slot_id) => {
                        let slot = self.func.slots[stack_slot_id.0].clone();

                        let expected_ty = match &slot.ty {
                            cyntax_mir::Ty::Ptr(ty) => ty.clone(),
                            _ => panic!(),
                        };
                        // Load the pointer out of the place
                        let loaded_ptr = self.insert(InstructionKind::StackLoad, vec![Operand::Place(*stack_slot_id)], Some(slot.ty.clone())).unwrap();
                        // Load the value out of the pointer
                        let loaded_value = self.insert(InstructionKind::Load, vec![Operand::Value(loaded_ptr)], Some(*expected_ty.clone())).unwrap();
                        Operand::Value(loaded_value)
                    }
                    _ => todo!(),
                }
            }
        }
    }
    fn current_block_has_terminator(&mut self) -> bool {
        for instruction in &self.func.blocks[self.current_block.0].instructions {
            if instruction.kind.is_terminator() {
                return true;
            }
        }
        return false;
    }
    fn next_value_id(&mut self) -> usize {
        let next_id = self.next_id;

        self.next_id += 1;
        next_id
    }
    pub fn stack_store(&mut self, slot: StackSlotId, value: Operand) {
        self.insert(mir::InstructionKind::StackStore, vec![Operand::Place(slot), value], None);
    }
    pub fn const_i64(&mut self, value: i64) -> Value {
        self.insert(cyntax_mir::InstructionKind::Const, vec![mir::Operand::Constant(value)], Some(mir::Ty::I64)).unwrap()
    }
    pub fn add(&mut self, lhs: Operand, rhs: Operand) -> Value {
        self.insert(InstructionKind::Add, vec![lhs, rhs], Some(mir::Ty::I64)).unwrap()
    }
    pub fn insert(&mut self, kind: InstructionKind, inputs: Vec<Operand>, output: Option<mir::Ty>) -> Option<mir::Value> {
        if kind.is_terminator() && self.current_block_has_terminator() {
            // panic!("cannot add terminator while block already has one!")
            // APPARENTLY THIS IS FINE IN C??????
            return None;
        }
        let output = output.map(|ty| {
            let id = self.next_value_id();
            mir::Value { id, ty }
        });
        let ins = Instruction { inputs, output: output.clone(), kind };
        self.func.blocks[self.current_block.0].instructions.push(ins);

        output
    }
}
