use std::{collections::HashMap, fmt::Display, ops::Deref};

use cyntax_common::{ctx::ParseContext, span, spanned::Spanned};
use cyntax_hir::{self as hir, HirId, HirMap, StructTypeKind};
use cyntax_mir::{self as mir, BasicBlock, BlockId, Instruction, InstructionKind, Operand, Place, StackSlotId, Value};
use cyntax_parser::ast::InfixOperator;

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
            cyntax_hir::StatementKind::Return(None) => {
                self.insert(InstructionKind::Return, vec![], None);
            }
            cyntax_hir::StatementKind::While(expression, statement) => {
                let header = self.allocate_block();
                let body = self.allocate_block();
                let cont = self.allocate_block();

                self.insert(InstructionKind::Jump, vec![cyntax_mir::Operand::BlockId(header)], None);
                self.current_block = header;
                let condition = self.lower_expression(expression);
                self.insert(InstructionKind::JumpIf, vec![condition, Operand::BlockId(body), Operand::BlockId(cont)], None);

                self.current_block = body;
                self.lower_statement(statement);
                self.insert(InstructionKind::Jump, vec![Operand::BlockId(header)], None);

                self.current_block = cont;
            }
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
                match spanned.value.suffix.width {
                    cyntax_parser::constant::Width::Long => cyntax_mir::Operand::Value(self.const_i64(num)),
                    cyntax_parser::constant::Width::LongLong => {
                        todo!()
                    }
                    cyntax_parser::constant::Width::None => cyntax_mir::Operand::Value(self.const_i32(num)),
                }
            }

            // TODO: Figure out phi/block params, creating an intermediate stack value isnt optimal
            cyntax_hir::ExpressionKind::BinaryOp(span!(InfixOperator::LogicalAnd), lhs, rhs) => {
                let zero = self.const_i8(0);
                let one = self.const_i8(1);

                let result_block = self.allocate_block();
                let check_rhs = self.allocate_block();

                let result_place = self.allocate_stack_slot(&cyntax_mir::Ty::I8);
                self.stack_store(result_place, cyntax_mir::Operand::Value(zero));

                {
                    let lhs = self.lower_expression(lhs);
                    let lhs_truthy = self.ins_eq(lhs, cyntax_mir::Operand::Value(one.clone()));

                    self.insert(
                        InstructionKind::JumpIf,
                        vec![cyntax_mir::Operand::Value(lhs_truthy), cyntax_mir::Operand::BlockId(check_rhs), cyntax_mir::Operand::BlockId(result_block)],
                        None,
                    );
                }

                // lhs was true, now check right side
                {
                    self.current_block = check_rhs;
                    let rhs = self.lower_expression(rhs);
                    let rhs_truthy = self.ins_eq(rhs, cyntax_mir::Operand::Value(one));
                    self.stack_store(result_place, cyntax_mir::Operand::Value(rhs_truthy));
                    self.insert(InstructionKind::Jump, vec![cyntax_mir::Operand::BlockId(result_block)], None);
                }
                // return block
                {
                    self.current_block = result_block;
                    let loaded_result = self.insert(InstructionKind::StackLoad, vec![Operand::Place(Place::new(result_place))], Some(cyntax_mir::Ty::I8)).unwrap();

                    cyntax_mir::Operand::Value(loaded_result)
                }
            }
            cyntax_hir::ExpressionKind::BinaryOp(spanned, lhs, rhs) => {
                let lhs = self.lower_expression(&lhs);
                let rhs = self.lower_expression(&rhs);

                match spanned.value {
                    InfixOperator::Add => Operand::Value(self.ins_add(lhs, rhs)),
                    InfixOperator::Subtract => todo!(),
                    InfixOperator::Multiply => todo!(),
                    InfixOperator::Divide => todo!(),
                    InfixOperator::Modulo => todo!(),
                    InfixOperator::Less => Operand::Value(self.ins_lt(lhs, rhs)),
                    InfixOperator::Greater => todo!(),
                    InfixOperator::LessEqual => todo!(),
                    InfixOperator::GreaterEqual => todo!(),
                    InfixOperator::Equal => Operand::Value(self.ins_eq(lhs, rhs)),
                    InfixOperator::NotEqual => todo!(),
                    InfixOperator::LogicalOr => todo!(),
                    InfixOperator::BitwiseAnd => todo!(),
                    InfixOperator::BitwiseOr => todo!(),
                    InfixOperator::BitwiseXor => todo!(),
                    InfixOperator::BitwiseShiftLeft => todo!(),
                    InfixOperator::BitwiseShiftRight => todo!(),
                    InfixOperator::Assign => {
                        self.ins_assign(lhs, rhs.clone());
                        rhs
                    }
                    InfixOperator::AddAssign => todo!(),
                    InfixOperator::SubtractAssign => todo!(),
                    InfixOperator::MultiplyAssign => todo!(),
                    InfixOperator::DivideAssign => todo!(),
                    InfixOperator::ModuloAssign => todo!(),
                    InfixOperator::BitwiseAndAssign => todo!(),
                    InfixOperator::BitwiseOrAssign => todo!(),
                    InfixOperator::BitwiseXorAssign => todo!(),
                    InfixOperator::BitwiseShiftRightAssign => todo!(),
                    InfixOperator::BitwiseShiftLeftAssign => todo!(),
                    InfixOperator::Access => todo!(),
                    InfixOperator::IndirectAccess => todo!(),
                    _ => unreachable!(),
                }
            }
            cyntax_hir::ExpressionKind::DeclarationReference(hir_id) => Operand::Place(Place::new(self.ss_map.get(hir_id).unwrap().clone())),
            cyntax_hir::ExpressionKind::Cast(ty, expression) => todo!(),
            cyntax_hir::ExpressionKind::MemberAccess(expression, spanned) => {
                let operand = self.lower_expression(&expression);
                let target = operand.as_place().unwrap();
                let slot_ty = self.func.slots[target.slot_id.0].ty.clone();
                dbg!(&slot_ty);

                let mut map = HashMap::new();
                let mut offset = 0;
                slot_ty.build_offset_map(&mut offset, &mut map);

                dbg!(spanned.value, &offset, &map);
                let offset = *map.get(&spanned.value).unwrap();
                dbg!(&offset, slot_ty.size_of());
                let field_place = Place { slot_id: target.slot_id, offset };

                Operand::Place(field_place)
            }
            cyntax_hir::ExpressionKind::AddressOf(expression) => {
                let operand = self.lower_expression(&expression);
                match &operand {
                    // Operand::Value(ref value) => {
                    //     let stack_slot = self.allocate_stack_slot(&value.ty);
                    //     self.stack_store(stack_slot, operand.clone());
                    //     let addr = self.insert(InstructionKind::StackAddr, vec![Operand::Place(stack_slot)], Some(cyntax_mir::Ty::Ptr(Box::new(value.ty.clone())))).unwrap();
                    //     Operand::Value(addr)
                    // }
                    Operand::Place(stack_slot) => {
                        let ty = self.func.slots[stack_slot.slot_id.0].ty.clone();
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
                    Operand::Place(stack_slot_id) => {
                        let slot = self.func.slots[stack_slot_id.slot_id.0].clone();

                        let expected_ty = match &slot.ty {
                            cyntax_mir::Ty::Ptr(ty) => ty.clone(),
                            _ => panic!(),
                        };
                        // Load the pointer out of the place
                        let loaded_ptr = self.insert(InstructionKind::StackLoad, vec![Operand::Place(stack_slot_id.clone())], Some(slot.ty.clone())).unwrap();
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
        self.insert(mir::InstructionKind::StackStore, vec![Operand::Place(Place::new(slot)), value], None);
    }
    pub fn const_i64(&mut self, value: i64) -> Value {
        self.insert(cyntax_mir::InstructionKind::Const(value), vec![], Some(mir::Ty::I64)).unwrap()
    }
    pub fn const_i32(&mut self, value: i64) -> Value {
        self.insert(cyntax_mir::InstructionKind::Const(value), vec![], Some(mir::Ty::I32)).unwrap()
    }
    pub fn const_i8(&mut self, value: i64) -> Value {
        self.insert(cyntax_mir::InstructionKind::Const(value), vec![], Some(mir::Ty::I8)).unwrap()
    }
    pub fn ins_add(&mut self, lhs: Operand, rhs: Operand) -> Value {
        self.insert(InstructionKind::Add, vec![lhs, rhs], Some(mir::Ty::I64)).unwrap()
    }
    pub fn ins_lt(&mut self, lhs: Operand, rhs: Operand) -> Value {
        self.insert(InstructionKind::LessThan, vec![lhs, rhs], Some(mir::Ty::I64)).unwrap()
    }
    pub fn ins_eq(&mut self, lhs: Operand, rhs: Operand) -> Value {
        self.insert(InstructionKind::Equal, vec![lhs, rhs], Some(mir::Ty::I64)).unwrap()
    }
    // pub fn ins_land(&mut self, lhs: Operand, rhs: Operand) -> Value {
    //     // self.insert(InstructionKind::LogicalAnd, vec![lhs, rhs], Some(mir::Ty::I64)).unwrap()
    // }
    pub fn ins_assign(&mut self, target: Operand, rhs: Operand) {
        match target {
            Operand::Value(value) => {
                self.insert(mir::InstructionKind::Store, vec![Operand::Value(value), rhs.clone()], None);
            }
            Operand::Place(stack_slot_id) => {
                self.insert(mir::InstructionKind::StackStore, vec![Operand::Place(stack_slot_id), rhs.clone()], None);
            }
            _ => unreachable!(),
        }
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
