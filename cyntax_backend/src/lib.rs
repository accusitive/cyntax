use std::collections::HashMap;

use cranelift::{
    codegen::{
        Context, ValueLabelsRanges,
        ir::{self, SourceLoc, ValueLabel},
    },
    prelude::{isa::CallConv, *},
};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cyntax_common::ctx::{ParseContext, string_interner::symbol::SymbolU32};
use cyntax_mir::{BlockId, Operand, Place, PlaceKind};

pub struct CliffLower<'src> {
    pctx: &'src mut ParseContext,
    ctx: Context,
    module: ObjectModule,
    functions: HashMap<SymbolU32, FuncId>,
}
impl<'src> CliffLower<'src> {
    pub fn new(pctx: &'src mut ParseContext) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        // flag_builder.set("opt_level", "speed_and_size").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
        let builder = ObjectBuilder::new(isa, "module", cranelift_module::default_libcall_names()).unwrap();
        let module = cranelift_object::ObjectModule::new(builder);
        let ctx = module.make_context();
        Self { pctx, ctx, module, functions: HashMap::new() }
    }
    pub fn lower(mut self, tu: &cyntax_mir::TranslationUnit) {
        println!("====mir done. cliff below====");
        self.lower_translation_unit(tu);
        let obj = self.module.finish();

        std::fs::write("./target/cyntax/build.o", obj.emit().unwrap()).unwrap();
    }
    pub fn lower_translation_unit(&mut self, tu: &cyntax_mir::TranslationUnit) {
        self.declare_functions(&tu);

        for func in &tu.functions {
            self.module.clear_context(&mut self.ctx);
            self.lower_function(func);
        }
    }
    pub fn declare_functions(&mut self, tu: &cyntax_mir::TranslationUnit) {
        for func in &tu.functions {
            let mut s = Signature::new(isa::CallConv::SystemV);
            Self::setup_abi(func, &mut s);
            let func_id = self.module.declare_function(self.pctx.res(func.name), Linkage::Export, &s).unwrap();
            self.functions.insert(func.name, func_id);
        }
    }
    fn setup_abi(func: &cyntax_mir::Function, signature: &mut Signature) {
        let t = Self::cliff_ty(func.ty.as_ref().unwrap());
        signature.call_conv = CallConv::SystemV;
        signature.returns.push(AbiParam::new(t));
        for param_ty in func.params.as_ref().unwrap() {
            match param_ty {
                cyntax_mir::Ty::Struct(_) => {
                    let struct_size = param_ty.size_of();
                    signature.params.push(AbiParam {
                        value_type: types::I64,
                        purpose: ir::ArgumentPurpose::StructArgument(struct_size),
                        extension: ir::ArgumentExtension::None,
                    });
                }
                _ => {
                    signature.params.push(AbiParam {
                        value_type: Self::cliff_ty(param_ty),
                        purpose: ir::ArgumentPurpose::Normal,
                        extension: ir::ArgumentExtension::None,
                    });
                }
            }
        }
    }
    pub fn lower_function(&mut self, func: &cyntax_mir::Function) {
        let mut func_ctx = FunctionBuilderContext::new();
        self.ctx.func.clear();
        Self::setup_abi(func, &mut self.ctx.func.signature);

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut func_ctx);
        builder.func.collect_debug_info();

        let mut slot_map = HashMap::new();
        let mut ins_map: HashMap<usize, Value> = HashMap::new();
        let mut block_map: HashMap<usize, Block> = HashMap::new();

        let mut slot_id = 0;
        for slot in &func.slots {
            slot_map.insert(slot_id, builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, slot.size, 0)));
            slot_id += 1;
        }
        let mut block_id = 0;
        for _ in &func.blocks {
            let block = builder.create_block();
            block_map.insert(block_id, block);
            block_id += 1;
        }

        block_id = 0;
        let mut entry = None;
        for bb in &func.blocks {
            let block = *block_map.get(&block_id).unwrap();
            if bb.entry {
                builder.append_block_params_for_function_params(block);
                entry = Some(block);
            }
            builder.switch_to_block(block);

            for ins in &bb.instructions {
                match ins.kind {
                    cyntax_mir::InstructionKind::Add => {
                        let lhs = Self::read_rvalue(&ins.inputs[0], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        let rhs = Self::read_rvalue(&ins.inputs[1], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.ins().iadd(lhs, rhs));
                    }
                    cyntax_mir::InstructionKind::LessThan => {
                        let lhs = Self::read_rvalue(&ins.inputs[0], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        let rhs = Self::read_rvalue(&ins.inputs[1], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs));
                    }
                    cyntax_mir::InstructionKind::Equal => {
                        let lhs = Self::read_rvalue(&ins.inputs[0], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        let rhs = Self::read_rvalue(&ins.inputs[1], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.ins().icmp(IntCC::Equal, lhs, rhs));
                    }
                    cyntax_mir::InstructionKind::Const(value) => {
                        let ty = &ins.output.as_ref().unwrap().ty;
                        let value = builder.ins().iconst(Self::cliff_ty(ty), value);
                        ins_map.insert(ins.output.as_ref().unwrap().id, value);
                    }
                    cyntax_mir::InstructionKind::JumpIf => {
                        let condition = Self::read_rvalue(&ins.inputs[0], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);

                        let then = ins.inputs[1].as_block_id().unwrap();
                        let then_block = block_map.get(&then.0).unwrap();

                        let elze = ins.inputs[2].as_block_id().unwrap();
                        let elze_block = block_map.get(&elze.0).unwrap();

                        builder.ins().brif(condition, *then_block, &[], *elze_block, &[]);
                    }
                    cyntax_mir::InstructionKind::Jump => {
                        let block_id = ins.inputs[0].as_block_id().unwrap();
                        let block = block_map.get(&block_id.0).unwrap();
                        builder.ins().jump(*block, &[]);
                    }
                    cyntax_mir::InstructionKind::Return => {
                        builder.ins().return_(&[]);
                    }
                    cyntax_mir::InstructionKind::ReturnValue => {
                        let value = Self::read_rvalue(&ins.inputs[0], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        // let value = builder.block_params(entry.unwrap())[0];
                        builder.ins().return_(&[value]);
                    }
                    cyntax_mir::InstructionKind::StackStore => {
                        if let Operand::Place(place) = &ins.inputs[0] {
                            let ss = slot_map.get(&place.as_slot().unwrap().0).unwrap();
                            let value = Self::read_rvalue(&ins.inputs[1], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                            builder.ins().stack_store(value, *ss, place.offset);
                        } else {
                            panic!("First operand to store must be a place")
                        }
                    }
                    cyntax_mir::InstructionKind::Store => {
                        let address = Self::read_rvalue(&ins.inputs[0], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        let data = Self::read_rvalue(&ins.inputs[1], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);

                        builder.ins().store(MemFlags::new(), address, data, 0);
                    }
                    cyntax_mir::InstructionKind::Load => {
                        let value = Self::read_rvalue(&ins.inputs[0], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        let t = Self::cliff_ty(&ins.output.as_ref().unwrap().ty);
                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.ins().load(t, MemFlags::new(), value, 0));
                    }
                    cyntax_mir::InstructionKind::StackLoad => {
                        let place = ins.inputs[0].as_place().unwrap().clone();

                        let slot = slot_map.get(&place.as_slot().unwrap().0).unwrap();
                        let expected = Self::cliff_ty(&ins.output.as_ref().unwrap().ty.clone());

                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.ins().stack_load(expected, *slot, place.offset));
                    }
                    cyntax_mir::InstructionKind::StackAddr => {
                        let slot = slot_map.get(&ins.inputs[0].as_place().unwrap().as_slot().unwrap().0).unwrap();
                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.ins().stack_addr(ir::types::I64, *slot, 0));
                    }
                    cyntax_mir::InstructionKind::Argument(idx) => {
                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.block_params(entry.unwrap())[idx]);
                    }
                    cyntax_mir::InstructionKind::Call => {
                        let addr = Self::read_rvalue(&ins.inputs[0], &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map);
                        // let arguments = &ins.inputs[1..];

                        // let mut arg_tys = vec![];
                        // for arg in arguments {
                        //     let v = arg.as_value().unwrap();
                        //     arg_tys.push(v.ty.clone());
                        // }
                        dbg!(&addr);

                        let mut s = self.module.make_signature();
                        let output = &ins.output.as_ref().unwrap().ty;
                        // for arg in &ins.inputs[1..] {
                        //     match arg {
                        //         Operand::Value(value) => {
                        //             s.params.push(AbiParam { value_type: Self::cliff_ty(&value.ty), purpose: ir::ArgumentPurpose::Normal, extension: ir::ArgumentExtension::None });
                        //         },
                        //         Operand::Place(place) => {
                        //             match place.kind {
                        //                 PlaceKind::Slot(stack_slot_id) => {
                        //                     let slot = slot_map.get(&stack_slot_id.0).unwrap();
                        //                 },
                        //                 PlaceKind::Argument(_) => todo!(),
                        //                 PlaceKind::Function(_) => todo!(),
                        //                 PlaceKind::Parameter(_) => todo!(),
                        //             }
                        //         },
                        //         Operand::BlockId(block_id) => todo!(),
                        //         Operand::FunctionIdentifier(symbol_u32) => todo!(),
                        //     }
                        // }
                        let args = ins.inputs[1..].iter().map(|a| Self::read_rvalue(a, &mut self.module, &self.functions, &func.slots, &slot_map, &mut builder, &ins_map)).collect::<Vec<_>>();

                        // for arg in args {
                        //     s.params.push(AbiParam { value_type: Self::cliff_ty(arg), purpose: (), extension: () });
                        // }
                        s.returns.push(AbiParam {
                            value_type: Self::cliff_ty(output),
                            purpose: ir::ArgumentPurpose::Normal,
                            extension: ir::ArgumentExtension::None,
                        });
                        let sr = builder.import_signature(s);

                        let inst = builder.ins().call_indirect(sr, addr, &args);
                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.inst_results(inst)[0]);
                    }
                    cyntax_mir::InstructionKind::FuncAddr => {
                        let func_identifier = ins.inputs[0].as_function_identifier().unwrap();
                        let func_id = *self.functions.get(func_identifier).unwrap();
                        let func_ref = self.module.declare_func_in_func(func_id, builder.func);

                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.ins().func_addr(ir::types::I64, func_ref));
                    }
                }
            }
            block_id += 1;
        }

        println!("{}", self.ctx.func.display());
        let func_id = *self.functions.get(&func.name).unwrap();
        // let func_id = self.module.declare_function(self.pctx.res(func.name), Linkage::Export, &self.ctx.func.signature).unwrap();
        match self.module.define_function(func_id, &mut self.ctx) {
            Ok(ok) => {}
            Err(e) => {
                panic!("{:#?}", e)
            }
        }
    }
    fn cliff_ty(ty: &cyntax_mir::Ty) -> ir::Type {
        match ty {
            cyntax_mir::Ty::U8 => ir::types::I8,
            cyntax_mir::Ty::I8 => ir::types::I8,
            cyntax_mir::Ty::U16 => ir::types::I16,
            cyntax_mir::Ty::I16 => ir::types::I16,
            cyntax_mir::Ty::U32 => ir::types::I32,
            cyntax_mir::Ty::I32 => ir::types::I32,
            cyntax_mir::Ty::U64 => ir::types::I64,
            cyntax_mir::Ty::I64 => ir::types::I64,
            cyntax_mir::Ty::F32 => ir::types::F32,
            cyntax_mir::Ty::F64 => ir::types::F64,
            cyntax_mir::Ty::Struct(struct_fields) => todo!(),
            cyntax_mir::Ty::Ptr(_) => ir::types::I64,
            cyntax_mir::Ty::Function { .. } => unreachable!(),
        }
    }
    fn read_rvalue(o: &Operand, module: &mut ObjectModule, functions: &HashMap<SymbolU32, FuncId>, slots: &[cyntax_mir::Slot], slot_map: &HashMap<usize, ir::StackSlot>, builder: &mut FunctionBuilder<'_>, ins_map: &HashMap<usize, Value>) -> Value {
        match o {
            Operand::Place(Place { kind: PlaceKind::Slot(slot_id), offset }) => {
                let ss = slot_map.get(&slot_id.0).unwrap();
                let slot_ty = &slots[slot_id.0];
                let field_ty = slot_ty.ty.type_at_offset(*offset);
                let v = builder.ins().stack_load(Self::cliff_ty(field_ty), *ss, *offset);
                v
            }
            Operand::Place(Place {
                kind: PlaceKind::Function((func_identifier, func_hir_id)),
                offset,
            }) => {
                let func_id = *functions.get(func_identifier).unwrap();
                let func_ref = module.declare_func_in_func(func_id, builder.func);

                builder.ins().func_addr(ir::types::I64, func_ref)
            }

            Operand::Value(value) => *ins_map.get(&value.id).unwrap(),
            // Operand::Constant(value) => builder.ins().iconst(ir::types::I32, *value),
            Operand::FunctionIdentifier(fnid) => {
                let func_id = *functions.get(fnid).unwrap();
                let func_ref = module.declare_func_in_func(func_id, builder.func);
                builder.ins().func_addr(ir::types::I64, func_ref)
            }

            // cyntax_mir::InstructionKind::Argument(idx) => {
            //     ins_map.insert(ins.output.as_ref().unwrap().id, builder.block_params(entry.unwrap())[idx]);
            // }
            Operand::BlockId(block_id) => panic!("??"),
            a => todo!("taking rvalue of {:?} not impl", o),
        }
    }
    // fn read_rvalue_address(o: &Operand, slot_map: &HashMap<usize, ir::StackSlot>, builder: &mut FunctionBuilder<'_>, ins_map: &HashMap<usize, Value>) -> Value {
    //     match o {
    //         Operand::Place(Place { kind: PlaceKind::Slot(slot_id), offset }) => {
    //             let ss = slot_map.get(&slot_id.0).unwrap();
    //             builder.ins().stack_addr(ir::types::I64, *ss, *offset)
    //         }
    //         Operand::Place(Place { kind: PlaceKind::Argument(arg_idx), offset }) => {
    //             builder.block_params()
    //         }
    //         _ => panic!("Can only take address of a Place"),
    //     }
    // }
}
