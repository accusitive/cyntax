use std::{collections::HashMap, fmt::Display, ops::Range};

use chumsky::{Parser, prelude::just, text};

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
struct TranslationUnit {
    items: Vec<TypeDefinition>,
}
pub struct TypeDefinition {
    identifier: String,
}
enum Token {}
type Span = Range<usize>;

type Spanned<T> = (T, Span);

fn lex<'a>() -> impl Parser<'a, str, Vec<Spanned<Token>>> {}
fn parse<'a>() -> impl Parser<'a, &'a str, Vec> {}
fn main() {

    // let int = Ty { generic_params: vec![], kind: TyKind::Int };
    // let float = Ty { generic_params: vec![], kind: TyKind::Float };

    // let wraps_int = Ty {
    //     generic_params: vec![GenericParameter {
    //         c: 'T',
    //         constraint: Some(Pattern::OneOf(vec![int.clone()])),
    //     }],
    //     kind: TyKind::Generic(0),
    // };

    // println!("{}", wraps_int.insantiate(&vec![float.clone()]));
}
impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Self { generic_params: vec![], kind }
    }
    fn compatible(&self, param: &GenericParameter, arg: &Ty) -> bool {
        if let Some(constraint) = &param.constraint {
            match constraint {
                Pattern::OneOf(items) => items.contains(&arg),
            }
        } else {
            true
        }
    }
    pub fn check_constraints(&self, generic_arguments: &Vec<Ty>) {
        match &self.kind {
            TyKind::Int => todo!(),
            TyKind::Float => todo!(),
            TyKind::Generic(index) => {
                assert!(self.compatible(&self.generic_params[*index], &generic_arguments[*index]));
            }
            TyKind::Enum { variants } => todo!(),
            TyKind::Typename { generic_arguments, ty } => {
                todo!()
            }
        }
    }
    pub fn insantiate(&self, generic_arguments: &Vec<Ty>) -> Self {
        self.check_constraints(generic_arguments);
        match &self.kind {
            TyKind::Int | TyKind::Float => self.clone(),
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
#[derive(Debug, Clone, PartialEq, Eq)]
struct Ty {
    generic_params: Vec<GenericParameter>,
    kind: TyKind,
    // predicate: ()
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct GenericParameter {
    c: char,
    constraint: Option<Pattern>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum Pattern {
    OneOf(Vec<Ty>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum TyKind {
    Int,
    Float,
    Generic(usize),
    Enum { variants: HashMap<String, Ty> },
    Typename { generic_arguments: Vec<Ty>, ty: Box<Ty> },
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TyKind::Int => write!(f, "int"),
            TyKind::Float => write!(f, "float"),

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
