use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub data: (),
    pub functions: Vec<Function>,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: (),
    // size
    pub slots: Vec<usize>,
    pub blocks: Vec<BasicBlock>,
}
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instructions: Vec<Instruction>,
}
#[derive(Debug, Clone)]
pub enum Ty {
    U8,
    I8,

    U16,
    I16,

    U32,
    I32,

    U64,
    I64,

    F32,
    F64,

    Struct,
    Ptr(Box<Self>),
}
#[derive(Debug, Clone)]
pub struct Value {
    pub id: usize,
    pub ty: Ty,
}
#[derive(Debug, Clone)]
pub enum Operand {
    Value(Value),
    Place(StackSlotId),
    Constant(i64),
}
#[derive(Debug, Clone)]
pub struct Instruction {
    pub inputs: Vec<Operand>,
    pub output: Option<Value>,
    pub kind: InstructionKind,
}
#[derive(Debug, Clone)]
pub enum InstructionKind {
    Add,
    Store,
    Const,
}
#[derive(Debug, Clone)]
pub struct StackSlotId(pub usize);
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "fn")?;
        let mut slot_id = 0;
        for size in &self.slots {
            writeln!(f, "\tslot{slot_id} [{} bytes];", size)?;
            slot_id += 1;
        }
        let mut block_id = 0;

        for block in &self.blocks {
            writeln!(f, "block{block_id}:")?;
            for ins in &block.instructions {
                let id = ins.output.as_ref().map(|v| format!("{:?}", v.id)).unwrap_or(String::new());
                write!(f, "\t{}={:?} (", id, ins.kind)?;
                for op in &ins.inputs {
                    match op {
                        Operand::Value(value) => {
                            write!(f, "value:{}", value.id)?;
                        }
                        Operand::Place(stack_slot_id) => {
                            write!(f, "stack:{}", stack_slot_id.0)?;
                        }
                        Operand::Constant(val) => {
                            write!(f, "const:{}", val)?;
                        }
                    }
                    write!(f, ", ")?;
                }
                write!(f, ")\n")?;

            }

            block_id += 1;
        }

        Ok(())
    }
}
