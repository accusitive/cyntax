use std::ops::Deref;

use cyntax_common::spanned::{Location, Spanned};
use cyntax_errors::{Diagnostic, errors::SimpleError};
use cyntax_hir::{FunctionParameter, HirId, HirNode, ParsedDeclarationSpecifiers, SpecifierQualifiers, StructField, StructType, StructTypeKind, Ty, TyKind, TyQualifiers, TypeSpecifierStateMachine};
use cyntax_parser::{
    PResult,
    ast::{self, DeclarationSpecifier, SpecifierQualifier},
};

use crate::AstLower;

impl<'src, 'hir> AstLower<'src, 'hir> {
    pub fn lower_declaration_ty_specifiers(&mut self, specifiers: &[Spanned<DeclarationSpecifier>]) -> PResult<ParsedDeclarationSpecifiers> {
        let mut base_type = TypeSpecifierStateMachine::None;
        let mut qualifier = TyQualifiers { conzt: false, restrict: false, volatile: false };
        let mut class = None;

        let mut last_location = Location::new();

        for specifier in specifiers {
            let loc = specifier.location.clone();
            last_location = loc.clone();
            match &specifier.value {
                ast::DeclarationSpecifier::StorageClass(storage_class_specifier) => match storage_class_specifier {
                    _ if class.is_some() => return Err(SimpleError(specifier.location.clone(), format!("already have a storage class")).into_codespan_report()),
                    storage_class => class = Some(storage_class),
                },
                ast::DeclarationSpecifier::TypeSpecifier(type_specifier) => match type_specifier {
                    ast::TypeSpecifier::Void => base_type = base_type.void(loc)?,
                    ast::TypeSpecifier::Char => base_type = base_type.char(loc)?,
                    ast::TypeSpecifier::Short => base_type = base_type.short(loc)?,
                    ast::TypeSpecifier::Int => base_type = base_type.int(loc)?,
                    ast::TypeSpecifier::Long => base_type = base_type.long(loc)?,
                    ast::TypeSpecifier::Float => base_type = base_type.float(loc)?,
                    ast::TypeSpecifier::Double => base_type = base_type.double(loc)?,
                    ast::TypeSpecifier::Signed => base_type = base_type.signed(loc)?,
                    ast::TypeSpecifier::Unsigned => base_type = base_type.unsigned(loc)?,
                    ast::TypeSpecifier::Bool => base_type = base_type.bool(loc)?,
                    ast::TypeSpecifier::TypedefName(typedef_name) => {
                        let t = self.find_typedef_in_scope(&loc.to_spanned(*typedef_name))?;

                        base_type = base_type.typedef_name(loc, t)?;
                    }
                    ast::TypeSpecifier::Struct(specifier) => {
                        let id = match &specifier.tag {
                            Some(tag) => {
                                if let Ok(s) = self.find_struct_in_scope(tag) {
                                    s
                                } else {
                                    self.lower_struct_ty_specifier(specifier)?
                                }
                            }
                            // Some(tag) if specifier.declarations.is_none() => self.find_struct_in_scope(tag)?,
                            _ => self.lower_struct_ty_specifier(specifier)?,
                        };

                        // let id =
                        base_type = base_type.struct_or_union(loc, id)?;
                    }
                    x => unimplemented!("{x:?}"),
                },
                ast::DeclarationSpecifier::TypeQualifier(type_qualifier) => match type_qualifier {
                    ast::TypeQualifier::Const => qualifier.conzt = true,
                    ast::TypeQualifier::Restrict => qualifier.restrict = true,
                    ast::TypeQualifier::Volatile => qualifier.volatile = true,
                },
                ast::DeclarationSpecifier::FunctionSpecifier(function_specifier) => todo!(),
            }
        }
        if let TypeSpecifierStateMachine::None = base_type {
            return Err(SimpleError(last_location, format!("must have at least 1 type specifier")).into_codespan_report());
        }

        Ok(ParsedDeclarationSpecifiers {
            class: class.cloned(),
            specifiers: base_type,
            qualifier: qualifier,
        })
    }
    pub fn lower_ty_specifiers_qualifiers(&mut self, specifiers: &[Spanned<SpecifierQualifier>]) -> PResult<SpecifierQualifiers> {
        let mut base_type = TypeSpecifierStateMachine::None;
        let mut qualifier = TyQualifiers { conzt: false, restrict: false, volatile: false };

        let mut last_location = Location::new();

        for specifier in specifiers {
            let loc = specifier.location.clone();
            last_location = loc.clone();
            match &specifier.value {
                ast::SpecifierQualifier::Specifier(type_specifier) => match type_specifier {
                    ast::TypeSpecifier::Void => base_type = base_type.void(loc)?,
                    ast::TypeSpecifier::Char => base_type = base_type.char(loc)?,
                    ast::TypeSpecifier::Short => base_type = base_type.short(loc)?,
                    ast::TypeSpecifier::Int => base_type = base_type.int(loc)?,
                    ast::TypeSpecifier::Long => base_type = base_type.long(loc)?,
                    ast::TypeSpecifier::Float => base_type = base_type.float(loc)?,
                    ast::TypeSpecifier::Double => base_type = base_type.double(loc)?,
                    ast::TypeSpecifier::Signed => base_type = base_type.signed(loc)?,
                    ast::TypeSpecifier::Unsigned => base_type = base_type.unsigned(loc)?,
                    ast::TypeSpecifier::Bool => base_type = base_type.bool(loc)?,
                    ast::TypeSpecifier::TypedefName(typedef_name) => {
                        let t = self.find_typedef_in_scope(&loc.to_spanned(*typedef_name))?;

                        base_type = base_type.typedef_name(loc, t)?;
                    }
                    ast::TypeSpecifier::Struct(specifier) => {
                        let id = self.lower_struct_ty_specifier(specifier)?;
                        base_type = base_type.struct_or_union(loc, id)?;
                    }
                    x => unimplemented!("{x:?}"),
                },
                ast::SpecifierQualifier::Qualifier(type_qualifier) => match type_qualifier {
                    ast::TypeQualifier::Const => qualifier.conzt = true,
                    ast::TypeQualifier::Restrict => qualifier.restrict = true,
                    ast::TypeQualifier::Volatile => qualifier.volatile = true,
                },
            }
        }
        if let TypeSpecifierStateMachine::None = base_type {
            return Err(SimpleError(last_location, format!("must have at least 1 type specifier")).into_codespan_report());
        }

        Ok(SpecifierQualifiers { specifiers: base_type, qualifier })
    }
    fn lower_struct_ty_specifier(&mut self, specifier: &ast::StructOrUnionSpecifier) -> PResult<HirId> {
        let id = self.next_id();
        let struct_ty = StructType {
            id,
            tag: specifier.tag.clone(),
            kind: cyntax_hir::StructTypeKind::Incomplete,
        };
        // define the incomplete struct_ty in the hir map, so self referential structs work
        self.map.nodes.insert(id, HirNode::StructType(self.arena.alloc(struct_ty)));

        if let Some(tag) = &specifier.tag {
            if specifier.declarations.is_some() {
                self.define_struct_type(tag, id)?;
            } else {
                return Ok(self.find_struct_in_scope(tag)?);
            }
            // let _ = self.define_struct_type(tag, id);
        }

        if let Some(declarations) = &specifier.declarations {
            let mut fields = vec![];
            for declaration in declarations {
                let base = self.lower_ty_specifiers_qualifiers(&declaration.value.specifier_qualifiers)?.into();
                for struct_declarator in &declaration.value.declarators {
                    let ty = self.lower_ty(&base, &struct_declarator.declarator.as_ref().unwrap())?;
                    let name = struct_declarator.declarator.as_ref().map(|decl| decl.value.get_identifier().expect("curious if this is ever none"));
                    fields.push(&*self.arena.alloc(StructField { ty, identifier: name }));
                }
            }
            let struct_ty = StructType {
                id,
                tag: specifier.tag.clone(),
                kind: StructTypeKind::Complete(self.arena.alloc_slice_copy(&fields)),
            };
            // Update the completed struct_ty in the hir map
            self.map.nodes.insert(id, cyntax_hir::HirNode::StructType(self.arena.alloc(struct_ty)));
        }
        Ok(id)
    }
    pub fn lower_ty(&mut self, base: &ParsedDeclarationSpecifiers, declarator: &Spanned<ast::Declarator>) -> PResult<&'hir Ty<'hir>> {
        let id = self.next_id();

        let mut kind = if let TypeSpecifierStateMachine::Typedef(hir_id) = base.specifiers {
            dbg!(&self.map.nodes[&hir_id]);
            panic!();
            // let decl = self.map.typedefs.get(&hir_id).unwrap();
            // cloning here is kinda sad but I don't see a way around it? we need a &ParsedDeclarationSpecifiers since we share it between declarators
            // decl.ty.kind.clone()
        } else {
            // cloning here is fine honestly, these types are so cheap
            TyKind::Base(SpecifierQualifiers {
                specifiers: base.specifiers.clone(),
                qualifier: base.qualifier.clone(),
            })
        };

        let mut next = Some(declarator);
        while let Some(next_declarator) = next {
            match &next_declarator.value {
                // Nop
                ast::Declarator::Abstract | ast::Declarator::Identifier(_) => {
                    next = None;
                }

                // *decl
                ast::Declarator::Pointer(ptr_info, inner) => {
                    let mut ptr = Some(&ptr_info.value);
                    while let Some(p) = ptr {
                        kind = TyKind::Pointer(p.type_qualifiers.clone(), Box::new(kind));
                        ptr = p.ptr.as_ref().map(|boxed| &boxed.value);
                    }
                    next = Some(inner.deref());
                }

                // (decl)
                ast::Declarator::Parenthesized(inner) => next = Some(&inner.deref()),

                // decl(int a)
                ast::Declarator::Function(inner, parameter_list) => {
                    let mut parameters = vec![];
                    for param in &parameter_list.parameters {
                        // let spec = DeclarationSpecifierParser::new(param.value.specifiers.iter(), &self.scopes, &mut self.map).parse()?;
                        let spec = self.lower_declaration_ty_specifiers(&param.value.specifiers)?;

                        let ty = self.lower_ty(&spec, param.value.declarator.as_ref().unwrap_or(&Spanned::new(Location::new(), ast::Declarator::Abstract)))?;
                        let name = param.value.declarator.as_ref().map(|declarator| declarator.value.get_identifier().map(|identifier| declarator.location.to_spanned(identifier))).flatten();
                        let id = self.next_id();
                        parameters.push(FunctionParameter { id, ty, identifier: name });
                    }
                    let parameters: &'hir _ = self.arena.alloc_slice_fill_iter(parameters.into_iter());

                    kind = TyKind::Function { return_ty: Box::new(kind), parameters };
                    next = Some(inner);
                }
                // decl[]
                ast::Declarator::Array {
                    base,
                    has_static: _,
                    has_star: _,
                    type_qualifiers: _,
                    expr,
                } => {
                    kind = TyKind::Array(Box::new(kind), self.lower_expression(expr.as_ref().unwrap())?);
                    next = Some(base);
                }
            }
        }
        let ty: &'hir _ = self.arena.alloc(Ty { id, kind: kind });
        Ok(ty)
    }
}
