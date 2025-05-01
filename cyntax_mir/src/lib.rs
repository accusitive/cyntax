use std::{collections::HashMap, fmt::Display};

use cyntax_parser::ast::Identifier;
#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub data: (),
    pub functions: Vec<Function>,
}
#[derive(Debug, Clone)]
pub struct Function {
    /// Deferred
    pub params: Option<Vec<Ty>>,
    pub name: Identifier,
    // Deferred
    pub ty: Option<Ty>,
    // size
    pub slots: Vec<Slot>,
    pub blocks: Vec<BasicBlock>,
}
#[derive(Debug, Clone)]
pub struct Slot {
    pub size: u32,
    pub ty: Ty,
}
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instructions: Vec<Instruction>,
    pub entry: bool,
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
    Padding(Ty),
}
impl StructField {
    pub fn get_ty(&self) -> &Ty {
        match self {
            StructField::Named(_, ty) => ty,
            StructField::Anonymous(ty) => ty,
            StructField::Padding(ty) => ty,
        }
    }
    pub fn get_name(&self) -> Option<Identifier> {
        match self {
            StructField::Named(symbol_u32, ty) => Some(*symbol_u32),
            StructField::Anonymous(ty) => None,
            StructField::Padding(ty) => None,
        }
    }
    fn is_struct(&self) -> bool {
        match self.get_ty() {
            Ty::Struct(struct_fields) => true,
            _ => false,
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
            Ty::Struct(fields) => fields.iter().map(|field| field.get_ty().size_of()).max().unwrap_or(1),
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
            Ty::Struct(fields) => fields.iter().map(|field| field.get_ty().size_of()).sum(),
            Ty::Ptr(_) => 8,
        }
    }
    pub fn flat_fields(&self) -> Vec<&Ty> {
        match self {
            Ty::Struct(struct_fields) => struct_fields.iter().map(|field| field.get_ty().flat_fields()).flatten().collect(),
            _ => vec![self],
        }
    }
    // recursively visit children to build out a map of Identifier : Byte offset
    pub fn build_offset_map(&self, offset: &mut i32, map: &mut HashMap<Identifier, i32>) {
        match self {
            Ty::Struct(struct_fields) => {
                for field in struct_fields {
                    if let Some(field_name) = field.get_name() {
                        map.insert(field_name, *offset);
                    }
                    field.get_ty().build_offset_map(offset, map);

                    if !field.is_struct() {
                        *offset += field.get_ty().size_of() as i32;
                    }
                }
            }
            _ => {}
        }
    }

    pub fn type_at_offset(&self, offset: i32) -> &Self {
        match self {
            Ty::Struct(_) => {
                let mut acc = 0;
                for t in self.flat_fields() {
                    acc += t.size_of() as i32;
                    if acc > offset {
                        return t;
                    }
                }
                panic!("offset {} out of bounds for {:?}", offset, self);
            }
            _ if offset > 0 => panic!("tried to get type at offset {} when self is {:?}", offset, self),
            _ => self,
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
    Place(Place),
    BlockId(BlockId),
}
#[derive(Debug, Clone)]
pub struct Place {
    pub kind: PlaceKind,
    pub offset: i32,
}
#[derive(Debug, Clone, Copy)]
pub enum PlaceKind {
    Slot(StackSlotId),
    Argument(usize),
    Function,
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
    LessThan,
    Equal,

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
    Argument(usize),
    Call,
}
#[derive(Debug, Clone, Copy)]
pub struct StackSlotId(pub usize);
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct BlockId(pub usize);
impl Operand {
    pub fn as_value(&self) -> Option<&Value> {
        if let Operand::Value(v) = self { Some(v) } else { None }
    }
    pub fn as_place(&self) -> Option<&Place> {
        if let Operand::Place(place) = self { Some(place) } else { None }
    }
    pub fn as_block_id(&self) -> Option<&BlockId> {
        if let Operand::BlockId(b) = self { Some(b) } else { None }
    }
}
impl Place {
    pub fn new_slot(slot_id: StackSlotId) -> Self {
        Self { kind: PlaceKind::Slot(slot_id), offset: 0 }
    }
    pub fn as_slot(&self) -> Option<StackSlotId> {
        match self.kind {
            PlaceKind::Slot(stack_slot_id) => Some(stack_slot_id),
            _ => None,
        }
    }
    pub fn as_arg(&self) -> Option<usize> {
        match self.kind {
            PlaceKind::Argument(arg) => Some(arg),
            _ => None,
        }
    }
    pub fn as_fn(&self) -> Option<()> {
        match self.kind {
            PlaceKind::Function => Some(()),
            _ => None,
        }
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
                write!(f, "\tval:{:7 } = {:?} (", id, ins.kind)?;
                for op in &ins.inputs {
                    match op {
                        Operand::Value(value) => {
                            write!(f, "val:{}", value.id)?;
                        }
                        Operand::Place(place) => {
                            let mut place_str = format!("{:?}", place.kind);
                            if place.offset > 0 {
                                place_str.push_str(&format!("+{}", place.offset));
                            }
                            write!(f, "place:{}", place_str)?;
                        }
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
