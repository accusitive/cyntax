use std::{collections::HashMap, fmt::Display};

// use std::collections::HashMap;
macro_rules! map {
    ( $( $key:expr => $val:expr ),* $(,)? ) => {{
        let mut map = ::std::collections::HashMap::new();
        $( map.insert($key.into(), $val.clone()); )*
        map
    }};
}

//type int = Int
// Type::Int

// type Value<T> = T;
// Type::generic(0)

// type Option<T> = Some(T) | None(T)
// Type::Sum{fields: ["Some": Type::Generic(0), "None": Generic(0)]}

// type NonZero<N: Int | Float> = N where match N { Int(n) if n > 0, Float(f) if  f > 0.0}
// N is considered "constrained" to be Int|Float
// Type::Pattern(Type::Generic(0), Pattern::Match([int_arm ,float_arm ]))

fn main() {
    let int = Ty { generic_params: vec![], kind: TyKind::Int };
    let val = Ty {
        generic_params: vec![GenericParameter { c: 'T', constraint: None }],
        kind: TyKind::Generic(0),
    };
    let result = Ty {
        generic_params: vec![GenericParameter { c: 'T', constraint: None }, GenericParameter { c: 'E', constraint: None }],
        kind: TyKind::Enum {
            variants: map!("Some" => Ty::new(TyKind::Generic(0)), "None" => Ty::new(TyKind::Generic(1))),
        },
    };
    let int_value_option = Ty {
        generic_params: vec![],
        kind: TyKind::Typename {
            generic_arguments: vec![int.clone()],
            ty: Box::new(val),
        },
    };
    dbg!(&int_value_option);
    dbg!(&int_value_option.insantiate(&vec![]));

    let int_int_result = Ty {
        generic_params: vec![],
        kind: TyKind::Typename {
            generic_arguments: vec![int.clone(), int.clone()],
            ty: Box::new(result),
        },
    };
    let iir_concrete = int_int_result.insantiate(&vec![]);
    dbg!(&int_int_result, &iir_concrete);
    println!("type iir = {}", int_int_result);
    println!("type iir_concrete = {}", iir_concrete);
}
impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Self { generic_params: vec![], kind }
    }
    pub fn check_constraints(&self, generic_arguments: &Vec<Ty>) {
        
    }
    pub fn insantiate(&self, generic_arguments: &Vec<Ty>) -> Self {
        match &self.kind {
            TyKind::Int => self.clone(),
            TyKind::Generic(generic_index) => {
                let arg = generic_arguments[*generic_index].clone();
                arg
            }
            TyKind::Enum { variants } => {
                let v = variants.iter().map(|(a, b)| (a.clone(), b.insantiate(generic_arguments))).collect();
                Ty {
                    generic_params: vec![],
                    kind: TyKind::Enum { variants: v },
                }
            }
            TyKind::Typename { generic_arguments, ty } => ty.insantiate(generic_arguments),
        }
    }
}
#[derive(Debug, Clone)]
struct Ty {
    generic_params: Vec<GenericParameter>,
    kind: TyKind,
}
#[derive(Debug, Clone)]
struct GenericParameter {
    c: char,
    constraint: Option<Pattern>,
}
impl GenericParameter {
    pub fn compatible(&self, arg: &Ty) -> bool {
        if let Some(constraint) = &self.constraint { false } else { true }
    }
}
#[derive(Debug, Clone)]
enum Pattern {
    OneOf(Vec<Ty>),
}
#[derive(Debug, Clone)]
enum TyKind {
    Int,
    Generic(usize),
    Enum { variants: HashMap<String, Ty> },
    Typename { generic_arguments: Vec<Ty>, ty: Box<Ty> },
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TyKind::Int => write!(f, "int"),
            TyKind::Generic(g) => write!(f, "{}", g),
            TyKind::Enum { variants } => {
                write!(f, "(")?;
                for (d, variant) in variants {
                    write!(f, "{}", d)?;
                    write!(f, "({})", variant)?;
                    write!(f, "|")?;
                }
                write!(f, ")")
            }
            TyKind::Typename { generic_arguments, ty } => {
                write!(f, "{}", ty)?;
                write!(f, "::<")?;
                for arg in generic_arguments {
                    write!(f, "{}", arg)?;
                    write!(f, ",",)?;
                }
                write!(f, ">")
            }
        }
    }
}
