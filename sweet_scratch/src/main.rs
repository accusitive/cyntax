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
        generic_params: vec!['T'],
        kind: TyKind::Generic(0),
    };
    let option = Ty {
        generic_params: vec!['T', 'E'],
        kind: TyKind::Enum {
            fields: map!("Some" => TyKind::Generic(0), "None" => TyKind::Generic(1)),
        },
    };
    // let int_option = Ty {
    //     generic_params: vec![],
    //     kind: TyKind::Typename{ generic_arguments: vec![int], ty: Box::new(option)},
    // };
    let int_value_option = Ty {
        generic_params: vec![],
        kind: TyKind::Typename {
            generic_arguments: vec![int.clone()],
            ty: Box::new(Ty {
                generic_params: vec![],
                kind: TyKind::Typename { generic_arguments: vec![int.clone()], ty: Box::new(option.clone()) },
            }),
        },
    };
    dbg!(&int_value_option);
}
#[derive(Debug, Clone)]
struct Ty {
    generic_params: Vec<char>,
    kind: TyKind,
}
#[derive(Debug, Clone)]
enum TyKind {
    Int,
    Generic(usize),
    Enum { fields: HashMap<String, TyKind> },
    Typename { generic_arguments: Vec<Ty>, ty: Box<Ty> },
}

// struct SumType {
//     generics: Vec<usize>,
//     fields: TypePattern
// }
// fn main() {
//     //   type Name<Params> = BaseType
//     //   [where match Param { Pattern [if Condition] }]
//     //   [| ...] // for sum types
//     let generics = map!("T" => None, "E" => None);
//     let result = Ty {
//         generic_parameters: generics,
//         base_type: TyKind::Sum(vec![
//             Variant {
//                 identifier: "Some".into(),
//                 ty: Ty {
//                     generic_parameters: HashMap::new(),
//                     base_type: TyKind::Generic("T".into()),
//                 },
//             },
//             Variant {
//                 identifier: "Err".into(),
//                 ty: Ty {
//                     generic_parameters: HashMap::new(),
//                     base_type: TyKind::Generic("E".into()),
//                 },
//             },
//         ]),
//     };

//     let int = Ty {
//         generic_parameters: HashMap::new(),
//         base_type: TyKind::Int,
//     };

//     let result_int_int = result.instantiate(&map!("T" => int, "E" => int));
//     dbg!(&result_int_int);

//     let sr = either();

//     let mapped = Ty{
//         generic_parameters: map!("A" => None, "B" => None),
//         base_type: TyKind::Pattern(TyPattern::Sum(vec!(result))),
//     };

//     dbg!(&mapped.instantiate(&map!("A" => int, "b" => int)));

//     // type Either<A,B> = Result<A, B>

//     // let result_result_int_int_int = result.instantiate(&map!("T" => result_int_int, "E" => int));
//     // dbg!(&result_result_int_int_int);
// }
// fn either() -> Ty {
//     let generic_constraints = map!("A" => None, "B" => None);

//     let result = Ty {
//         generic_parameters: generic_constraints,
//         base_type: TyKind::Sum(vec![
//             Variant {
//                 identifier: "Left".into(),
//                 ty: Ty {
//                     generic_parameters: HashMap::new(),
//                     base_type: TyKind::Generic("A".into()),
//                 },
//             },
//             Variant {
//                 identifier: "Right".into(),
//                 ty: Ty {
//                     generic_parameters: HashMap::new(),
//                     base_type: TyKind::Generic("B".into()),
//                 },
//             },
//         ]),
//     };
//     result
// }
// type Identifier = String;
// #[derive(Debug, Clone)]
// struct Variant {
//     identifier: Identifier,
//     ty: Ty,
// }
// #[derive(Debug, Clone)]
// enum TyKind {
//     Int,
//     Pattern(TyPattern),
//     Sum(Vec<Variant>),
//     // index
//     Generic(Identifier),
// }
// #[derive(Debug, Clone)]
// pub struct GenericConstraints {
//     allowed_types: TyPattern,
// }
// #[derive(Debug, Clone)]
// pub enum TyPattern {
//     Sum(Vec<Ty>),
//     ExactlyOne(Box<Ty>),

// }
// #[derive(Debug, Clone)]
// struct Ty {
//     generic_parameters: HashMap<Identifier, Option<GenericConstraints>>,
//     base_type: TyKind,
// }
// // impl GenericConstraints {
// //     pub fn is_allowed(&self, ty: &Ty) -> bool{
// //         match self.allowed_types {
// //             TyPattern::Sum(items) => todo!(),
// //             TyPattern::ExactlyOne(ty) => ty == ,
// //             TyPattern::Any => true,
// //         }
// //         true
// //     }
// // }
// impl Ty {
//     pub fn instantiate(&self, generic_arguments: &HashMap<Identifier, Ty>) -> Ty {
//         // check constraints
//         for ((identifier, constraints), (argument_identifier, ty)) in self.generic_parameters.iter().zip(generic_arguments.iter()) {
//             // if !constraints.is_allowed(&ty) {
//             //     panic!();
//             // }
//         }
//         let instantiated = self.base_type.instantiate(generic_arguments).unwrap();
//         Ty {
//             generic_parameters: HashMap::new(),
//             base_type: instantiated.base_type,
//         }
//     }
// }
// impl TyKind {
//     pub fn instantiate(&self, generic_arguments: &HashMap<Identifier, Ty>) -> Option<Ty> {
//         match self {
//             // TyKind::Pattern(pattern) => {
//             //     match pattern {
//             //         TyPattern::Sum(items) => {
//             //             Some(Ty { generic_parameters: HashMap::new(), base_type: TyKind::Sum() })
//             //         },
//             //         TyPattern::ExactlyOne(ty) => todo!(),
//             //     }
//             // }
//             TyKind::Int => None,
//             TyKind::Sum(ty_kinds) => {
//                 let instantiated_ty_kinds = ty_kinds
//                     .iter()
//                     .map(|variant| Variant {
//                         identifier: variant.identifier.clone(),
//                         ty: variant.ty.instantiate(generic_arguments),
//                     })
//                     .collect::<Vec<_>>();
//                 Some(Ty {
//                     generic_parameters: HashMap::new(),
//                     base_type: TyKind::Sum(instantiated_ty_kinds),
//                 })
//             }
//             TyKind::Generic(identifier) => Some(generic_arguments.get(identifier).unwrap().clone()),
//         }
//     }
// }
