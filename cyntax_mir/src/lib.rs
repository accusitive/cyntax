
use std::fmt::Display;

use cyntax_parser::ast::Identifier;
#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub data: (),
    pub functions: Vec<Function>,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub ty: Option<Ty>,
    // size
    pub slots: Vec<Slot>,
    pub blocks: Vec<BasicBlock>,
}
#[derive(Debug, Clone)]
pub struct Slot {
    pub size: u32,
    pub ty: Ty
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

    Struct(Vec<StructField>),
    Ptr(Box<Self>),
}
#[derive(Debug, Clone)]
pub enum StructField {
    Named(Identifier, Ty),
    Anonymous(Ty),
    Padding(Ty)
}
impl StructField {
    pub fn get_ty(&self) -> &Ty {
        match self {
            StructField::Named(_, ty) => ty,
            StructField::Anonymous(ty) => ty,
            StructField::Padding(ty) => ty
        }
    }
}
impl Ty {
    pub fn align_of(&self) -> u32 {
        match self {
            Ty::U8 => 1,
            Ty::I8 => 1,
            Ty::U16 => 2,
            Ty::I16 => 2,
            Ty::U32 => 4,
            Ty::I32 => 4,
            Ty::U64 => 8,
            Ty::I64 => 8,
            Ty::F32 => 4,
            Ty::F64 => 8,
            Ty::Struct(fields) => {
                fields.iter().map(|field| field.get_ty().size_of()).max().unwrap_or(0)
            }
            Ty::Ptr(_) => 8,
        }
    }
    pub fn size_of(&self) -> u32 {
        match self {
            Ty::U8 => 1,
            Ty::I8 => 1,
            Ty::U16 => 2,
            Ty::I16 => 2,
            Ty::U32 => 4,
            Ty::I32 => 4,
            Ty::U64 => 8,
            Ty::I64 => 8,
            Ty::F32 => 4,
            Ty::F64 => 8,
            Ty::Struct(fields) => {
                fields.iter().map(|field| field.get_ty().size_of()).sum()
            },
            Ty::Ptr(_) => 8,
        }
    }
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
    BlockId(BlockId),
}


#[derive(Debug, Clone)]
pub struct Instruction {
    pub inputs: Vec<Operand>,
    pub output: Option<Value>,
    pub kind: InstructionKind,
}
#[derive(Debug, Clone)]
pub enum InstructionKind {
    // Math
    Add,

    // Memory
    Load,
    Store,
    StackLoad,
    StackStore,
    StackAddr,

    Const(i64),

    // Control flow
    JumpIf,
    Jump,
    Return,
    ReturnValue,
}
#[derive(Debug, Clone, Copy)]
pub struct StackSlotId(pub usize);
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct BlockId(pub usize);
impl Operand {
    pub fn as_value(&self) -> Option<&Value> {
        if let Operand::Value(v) = self { Some(v) } else { None }
    }
    pub fn as_place(&self) -> Option<&StackSlotId> {
        if let Operand::Place(p) = self { Some(p) } else { None }
    }
    pub fn as_block_id(&self) -> Option<&BlockId> {
        if let Operand::BlockId(b) = self { Some(b) } else { None }
    }
}
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "fn")?;
        let mut slot_id = 0;
        for slot in &self.slots {
            writeln!(f, "\tslot:{slot_id} [{} bytes; {:?}];", slot.size, slot.ty)?;
            slot_id += 1;
        }
        let mut block_id = 0;

        for block in &self.blocks {
            writeln!(f, "block{block_id}:")?;
            for ins in &block.instructions {
                let id = ins.output.as_ref().map(|v| format!("{:?} ({:?})", v.id, v.ty)).unwrap_or(String::from("N/A"));
                write!(f, "\tval:{:3 } = {:?} (", id, ins.kind)?;
                for op in &ins.inputs {
                    match op {
                        Operand::Value(value) => {
                            write!(f, "val:{}", value.id)?;
                        }
                        Operand::Place(stack_slot_id) => {
                            write!(f, "slot:{}", stack_slot_id.0)?;
                        }
                        // Operand::Constant(val) => {
                        //     write!(f, "const:{}", val)?;
                        // }
                        Operand::BlockId(block_id) => {
                            write!(f, "bb:{}", block_id.0)?;
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

impl InstructionKind {
    pub fn is_terminator(&self) -> bool {
        match self {
            InstructionKind::JumpIf | InstructionKind::Jump | InstructionKind::Return | InstructionKind::ReturnValue => true,
            _ => false,
        }
    }
}
