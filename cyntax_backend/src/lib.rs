use std::collections::HashMap;

use cranelift::{
    codegen::{Context, ir},
    prelude::*,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cyntax_common::ctx::ParseContext;
use cyntax_mir::{BlockId, Operand};

pub struct CliffLower<'src> {
    pctx: &'src mut ParseContext,
    ctx: Context,
    module: ObjectModule,
}
impl<'src> CliffLower<'src> {
    pub fn new(pctx: &'src mut ParseContext) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
        let builder = ObjectBuilder::new(isa, "module", cranelift_module::default_libcall_names()).unwrap();
        let module = cranelift_object::ObjectModule::new(builder);
        let ctx = module.make_context();
        Self { pctx, ctx, module }
    }
    pub fn lower(mut self, tu: &cyntax_mir::TranslationUnit) {
        self.lower_translation_unit(tu);
        self.module.clear_context(&mut self.ctx);
        let obj = self.module.finish();
        std::fs::write("./target/cyntax/build.o", obj.emit().unwrap()).unwrap();
    }
    pub fn lower_translation_unit(&mut self, tu: &cyntax_mir::TranslationUnit) {
        for func in &tu.functions {
            self.lower_function(func);
        }
    }
    pub fn lower_function(&mut self, func: &cyntax_mir::Function) {
        let mut func_ctx = FunctionBuilderContext::new();
        self.ctx.func.signature.returns.push(AbiParam::new(types::I64));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut func_ctx);

        let mut slot_map = HashMap::new();
        let mut ins_map: HashMap<usize, Value> = HashMap::new();
        let mut block_map: HashMap<usize, Block> = HashMap::new();

        let mut slot_id = 0;
        for slot_size in &func.slots {
            slot_map.insert(slot_id, builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, *slot_size, 0)));
            slot_id += 1;
        }

        let mut block_id = 0;
        for _ in &func.blocks {
            let block = builder.create_block();
            block_map.insert(block_id, block);
            block_id += 1;
        }

        block_id = 0;
        for bb in &func.blocks {
            let block = *block_map.get(&block_id).unwrap();

            builder.switch_to_block(block);
            println!("handling block {:?}", block_id);

            for ins in &bb.instructions {
                match ins.kind {
                    cyntax_mir::InstructionKind::Add => {
                        let lhs = Self::get_value(&ins.inputs[0], &slot_map, &mut builder, &ins_map);
                        let rhs = Self::get_value(&ins.inputs[1], &slot_map, &mut builder, &ins_map);
                        ins_map.insert(ins.output.as_ref().unwrap().id, builder.ins().iadd(lhs, rhs));
                    }
                    cyntax_mir::InstructionKind::Store => {
                        if let Operand::Place(place) = &ins.inputs[0] {
                            let ss = slot_map.get(&place.0).unwrap();
                            let value = Self::get_value(&ins.inputs[1], &slot_map, &mut builder, &ins_map);
                            builder.ins().stack_store(value, *ss, 0);
                        } else {
                            panic!("First operand to store must be a place")
                        }
                    }
                    cyntax_mir::InstructionKind::Const => {
                        let i = Self::get_value(&ins.inputs[0], &slot_map, &mut builder, &ins_map);
                        ins_map.insert(ins.output.as_ref().unwrap().id, i);
                    }
                    cyntax_mir::InstructionKind::JumpIf => {
                        let condition = Self::get_value(&ins.inputs[0], &slot_map, &mut builder, &ins_map);

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
                    },
                    cyntax_mir::InstructionKind::ReturnValue => {
                        let value = Self::get_value(&ins.inputs[0], &slot_map, &mut builder, &ins_map);
                        builder.ins().return_(&[value]);
                    }
                }
            }
            block_id += 1;
        }

        // let forty_two = builder.ins().iconst(types::I32, 42);
        // builder.ins().return_(&[forty_two]);
        // builder.seal_all_blocks();

        let func_id = self.module.declare_function("main", Linkage::Export, &self.ctx.func.signature).unwrap();
        self.module.define_function(func_id, &mut self.ctx).unwrap();

        println!("{}", self.ctx.func.display());
    }
    fn get_value(o: &Operand, slot_map: &HashMap<usize, ir::StackSlot>, builder: &mut FunctionBuilder<'_>, ins_map: &HashMap<usize, Value>) -> Value {
        match o {
            Operand::Place(stack_slot_id) => {
                let ss = slot_map.get(&stack_slot_id.0).unwrap();
                builder.ins().stack_load(ir::types::I64, *ss, 0)
            }
            Operand::Value(value) => *ins_map.get(&value.id).unwrap(),
            Operand::Constant(value) => builder.ins().iconst(ir::types::I64, *value),
            Operand::BlockId(block_id) => panic!("??"),
        }
    }
}
