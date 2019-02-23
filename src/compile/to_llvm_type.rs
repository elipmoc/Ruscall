/*extern crate inkwell;

use compile::types::*;
use self::inkwell::*;
use self::inkwell::types;
use self::inkwell::types::{BasicType, AnyType};

impl Type {
    pub fn to_llvm_basic_type(&self) -> types::BasicTypeEnum {
        match self {
            Type::TCon { name } =>
                match name as &str {
                    "Int32" => types::IntType::i32_type().as_basic_type_enum(),
                    "Bool" => types::IntType::bool_type().as_basic_type_enum(),
                    _ => panic!("undefined!")
                },
            Type::TupleType(x) => x.to_llvm_type().as_basic_type_enum(),
            Type::TyVar(_) | Type::TGen(_, _) => Type::TupleType(Box::new(TupleType { element_tys: vec![] })).to_llvm_basic_type(),
            Type::LambdaType(x) => x.to_llvm_basic_type(),
            Type::StructType(x) => x.to_llvm_type().as_basic_type_enum(),
        }
    }

    pub fn to_llvm_any_type(&self, fn_pointer_flag: bool) -> types::AnyTypeEnum {
        match self {
            Type::TCon { name } =>
                match name as &str {
                    "Int32" => types::IntType::i32_type().as_any_type_enum(),
                    "Bool" => types::IntType::i8_type().as_any_type_enum(),
                    _ => panic!("undefined!")
                },
            Type::TupleType(x) => x.to_llvm_type().as_any_type_enum(),
            Type::TyVar(_) => Type::TupleType(Box::new(TupleType { element_tys: vec![] })).to_llvm_any_type(fn_pointer_flag),
            Type::LambdaType(x) => x.to_llvm_any_type(fn_pointer_flag),
            Type::StructType(x) => x.to_llvm_type().as_any_type_enum(),
            Type::TGen(_, _) => panic!("TGen error")
        }
    }
}

impl FuncType {
    pub fn to_llvm_type(&self) -> types::FunctionType {
        self.ret_type.to_llvm_basic_type()
            .fn_type(
                &self.param_types
                    .iter()
                    .map(|x| x.to_llvm_basic_type())
                    .collect::<Vec<_>>(),
                false,
            )
    }
}

impl TupleType {
    pub fn to_llvm_type(&self) -> types::StructType {
        types::StructType::struct_type(
            &self.element_tys
                .iter()
                .map(|x| x.to_llvm_basic_type())
                .collect::<Vec<_>>(),
            true,
        )
    }
}

impl LambdaType {
    pub fn to_llvm_any_type(&self, fn_pointer_flag: bool) -> types::AnyTypeEnum {
        if fn_pointer_flag {
            if let Some(env_ty) = self.env_ty.clone() {
                let env_llvm_ty = env_ty.to_llvm_type();
                let func_llvm_ty = self.func_ty.to_llvm_type().ptr_type(AddressSpace::Generic);
                types::StructType::struct_type(&vec![env_llvm_ty.as_basic_type_enum(), func_llvm_ty.as_basic_type_enum()], true).as_any_type_enum()
            } else {
                self.func_ty.to_llvm_type().ptr_type(AddressSpace::Generic).as_any_type_enum()
            }
        } else {
            self.func_ty.to_llvm_type().as_any_type_enum()
        }
    }

    pub fn to_llvm_basic_type(&self) -> types::BasicTypeEnum {
        if let Some(env_ty) = self.env_ty.clone() {
            let env_llvm_ty = env_ty.to_llvm_type();
            let func_llvm_ty = self.func_ty.to_llvm_type().ptr_type(AddressSpace::Generic);
            types::StructType::struct_type(&vec![env_llvm_ty.as_basic_type_enum(), func_llvm_ty.as_basic_type_enum()], true).as_basic_type_enum()
        } else {
            self.func_ty.to_llvm_type().ptr_type(AddressSpace::Generic).as_basic_type_enum()
        }
    }
}

impl StructType {
    pub fn to_llvm_type(&self) -> types::StructType {
        match self.ty {
            StructInternalType::TupleType(ref x) => x.to_llvm_type(),
            StructInternalType::RecordType(ref x) => {
                types::StructType::struct_type(
                    &x.element_tys
                        .iter()
                        .map(|(_, x)| x.to_llvm_basic_type())
                        .collect::<Vec<_>>(),
                    true,
                )
            }
        }
    }
}*/