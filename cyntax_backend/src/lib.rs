use cranelift::{codegen::Context, prelude::*};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cyntax_common::ctx::ParseContext;

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
        
    }
    pub fn lower_translation_unit(&mut self, tu: &cyntax_mir::TranslationUnit) {
        for func in &tu.functions {
            self.lower_function(func);
        }
    }
    pub fn lower_function(&mut self, func: &cyntax_mir::Function) {
        let mut func_ctx = FunctionBuilderContext::new();
        self.ctx.func.signature.returns.push(AbiParam::new(types::I32));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut func_ctx);

        for stack_slot in &func.slots {
            builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: 0,
                align_shift: 0,
            });
        }
        for block in &func.blocks {
            // builder.ins().call_indirect(SIG, callee, args)       
        }
        let func_id = self.module.declare_function("my_function", Linkage::Export, &self.ctx.func.signature).unwrap();
        self.module.define_function(func_id, &mut self.ctx).unwrap();
    }
}
