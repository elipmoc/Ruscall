extern crate inkwell;

use self::inkwell::*;
use self::inkwell::{values::BasicValue, values::AnyValue, types::BasicType, types::AnyType};
use super::ir::mir;
use super::semantic_analysis::type_env::TypeInfo;
use super::types::types::*;

pub struct CodeGenResult<'a> {
    pub file_name: &'a str,
    pub builder: builder::Builder,
    pub module: module::Module,
}

//コード生成する関数
impl mir::ProgramMir {
    pub fn code_gen(self, file_name: &str) -> CodeGenResult {
        //llvm初期化
        targets::Target::initialize_all(&targets::InitializationConfig::default());
        let builder = builder::Builder::create();
        let module = module::Module::create(file_name);

        let mut ty_info = self.ty_info;
        //外部関数宣言のコード化
        self.ex_dec_func_list
            .into_iter()
            .for_each(|x| ex_func_gen(x, &module, &mut ty_info));

        //関数宣言のコード化
        self.implicit_func_list.iter().map(|(_, x)| &x.func)
            .chain(self.explicit_func_list.iter().map(|x| &x.func))
            .for_each(|x| {
                let func_type = ty_info
                    .look_up_func_name(x.name.clone())
                    .to_llvm_any_type(false).into_function_type();

                use compile::mangling::mangle;
                println!("{}", mangle(&x.name, &ty_info.look_up_func_name(x.name.clone())));
                module.add_function(&x.name, func_type, Some(module::Linkage::External));
            });

        //関数定義のコード化
        self.implicit_func_list
            .into_iter().map(|(_, x)| x.func)
            .chain(self.explicit_func_list.into_iter().map(|x| x.func))
            .for_each(|func| func.code_gen(&module, &builder, &mut ty_info));

        if let Err(err_msg) = module.verify() {
            module.print_to_stderr();
            panic!("llvm error:{}", err_msg.to_string());
        }

        CodeGenResult {
            file_name,
            module,
            builder,
        }
    }
}

fn ex_func_gen(dec_func_ir: mir::DecFuncMir, module: &module::Module, ty_info: &mut TypeInfo) {
    use compile::types::show_type::ShowType;
    match ty_info.look_up_func_name(dec_func_ir.name.clone()) {
        Type::LambdaType(ty) =>
            module.add_function(&dec_func_ir.name, ty.to_llvm_any_type(false).as_any_type_enum().into_function_type(), Some(module::Linkage::External)),
        x => panic!("error!{}", x.show()),
    };
}

impl Type {
    fn to_llvm_basic_type(&self) -> types::BasicTypeEnum {
        match self {
            Type::TCon { name } =>
                match name as &str {
                    "Int32" => types::IntType::i32_type().as_basic_type_enum(),
                    "Bool" => types::IntType::bool_type().as_basic_type_enum(),
                    _ => panic!("undefined!")
                },
            Type::TupleType(x) => x.to_llvm_type().as_basic_type_enum(),
            Type::TyVar(_) => Type::TupleType(Box::new(TupleType {element_tys: vec![]})).to_llvm_basic_type(),
            Type::LambdaType(x) => x.to_llvm_basic_type(),
            Type::StructType(x) => x.to_llvm_type().as_basic_type_enum(),
        }
    }

    fn to_llvm_any_type(&self, fn_pointer_flag: bool) -> types::AnyTypeEnum {
        match self {
            Type::TCon { name } =>
                match name as &str {
                    "Int32" => types::IntType::i32_type().as_any_type_enum(),
                    "Bool" => types::IntType::i8_type().as_any_type_enum(),
                    _ => panic!("undefined!")
                },
            Type::TupleType(x) => x.to_llvm_type().as_any_type_enum(),
            Type::TyVar(_) =>Type::TupleType(Box::new(TupleType {element_tys: vec![]})).to_llvm_any_type(fn_pointer_flag),
            Type::LambdaType(x) => x.to_llvm_any_type(fn_pointer_flag),
            Type::StructType(x) => x.to_llvm_type().as_any_type_enum(),
        }
    }
}

impl FuncType {
    fn to_llvm_type(&self) -> types::FunctionType {
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
    fn to_llvm_type(&self) -> types::StructType {
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
    fn to_llvm_any_type(&self, fn_pointer_flag: bool) -> types::AnyTypeEnum {
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

    fn to_llvm_basic_type(&self) -> types::BasicTypeEnum {
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
    fn to_llvm_type(&self) -> types::StructType {
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
}

impl mir::ExprMir {
    fn get_ty(&self, ty_info: &mut TypeInfo) -> Type {
        match self {
            mir::ExprMir::NumMir(_) => Type::create_int32(),
            mir::ExprMir::BoolMir(_) => Type::create_bool(),
            mir::ExprMir::OpMir(_) => Type::create_int32(),
            mir::ExprMir::VariableMir(x) => ty_info.look_up(&x.ty_id),
            mir::ExprMir::IfMir(x) => ty_info.look_up(&x.ty_id),
            mir::ExprMir::GlobalVariableMir(x) => ty_info.look_up_func_name(x.id.clone()),
            mir::ExprMir::CallMir(x) => ty_info.look_up(&x.ty_id),
            mir::ExprMir::TupleMir(x) => ty_info.look_up(&x.ty_id),
            mir::ExprMir::TupleStructMir(_) => panic!("undefined"),
            mir::ExprMir::IndexPropertyMir(x) => ty_info.look_up(&x.ty_id),
            mir::ExprMir::NamePropertyMir(x) => ty_info.look_up(&x.ty_id),
            mir::ExprMir::LambdaMir(x) => ty_info.look_up(&x.ty_id),
        }
    }
}

impl mir::FuncMir {
    fn code_gen(self, module: &module::Module, builder: &builder::Builder, ty_info: &mut TypeInfo) {
        let function = module.get_function(&self.name).unwrap();
        let params = function.get_params();
        let entry_block = function.append_basic_block(&"entry");
        builder.position_at_end(&entry_block);
        let value = self.body.code_gen(&module, &builder, &params, ty_info, function);
        builder.build_return(Some(&value));
    }
}

impl mir::ExprMir {
    fn code_gen(
        self,
        module: &module::Module,
        builder: &builder::Builder,
        params: &Vec<values::BasicValueEnum>,
        ty_info: &mut TypeInfo,
        function: values::FunctionValue,
    ) -> values::BasicValueEnum {
        match self {
            mir::ExprMir::NumMir(num_ir) => types::IntType::i32_type().const_int(num_ir.num as u64, true).as_basic_value_enum(),
            mir::ExprMir::BoolMir(bool_ir) => types::IntType::bool_type().const_int(
                if bool_ir.bool { 1 } else { 0 },
                false,
            ).as_basic_value_enum(),
            mir::ExprMir::IfMir(x) => x.code_gen(module, builder, params, ty_info, function).as_basic_value(),
            mir::ExprMir::OpMir(op_ir) => op_ir.code_gen(module, builder, params, ty_info, function),
            mir::ExprMir::VariableMir(var_ir) => params[params.len() - var_ir.id - 1],
            mir::ExprMir::GlobalVariableMir(x) => x.code_gen(module).as_any_value_enum().into_pointer_value().as_basic_value_enum(),
            mir::ExprMir::TupleMir(x) => x.code_gen(module, builder, params, ty_info, function),
            mir::ExprMir::TupleStructMir(x) => x.tuple.code_gen(module, builder, params, ty_info, function),
            mir::ExprMir::LambdaMir(x) => x.code_gen(module, builder, params, ty_info),
            mir::ExprMir::CallMir(x) => x.code_gen(module, builder, params, ty_info, function),
            mir::ExprMir::IndexPropertyMir(x) => x.code_gen(module, builder, params, ty_info, function),
            mir::ExprMir::NamePropertyMir(x) => x.code_gen(module, builder, params, ty_info, function),
        }
    }
}

impl mir::GlobalVariableMir {
    fn code_gen(self, module: &module::Module) -> values::FunctionValue {
        module.get_function(&self.id).unwrap()
    }
}

impl mir::IfMir {
    fn code_gen(
        self,
        module: &module::Module,
        builder: &builder::Builder,
        params: &Vec<values::BasicValueEnum>,
        ty_info: &mut TypeInfo,
        function: values::FunctionValue,
    ) -> values::PhiValue {
        let cond_value = self.cond.code_gen(&module, &builder, &params, ty_info, function).into_int_value();
        let then_block = function.append_basic_block(&"then");
        let else_block = function.append_basic_block(&"else");
        let merge_block = function.append_basic_block(&"merge");
        builder.build_conditional_branch(cond_value, &then_block, &else_block);
        builder.position_at_end(&then_block);
        let t_value: &values::BasicValue = &self.t_expr.code_gen(&module, &builder, &params, ty_info, function);
        builder.build_unconditional_branch(&merge_block);
        let then_block = builder.get_insert_block().unwrap();
        builder.position_at_end(&else_block);
        let f_value: &values::BasicValue = &self.f_expr.code_gen(&module, &builder, &params, ty_info, function);
        builder.build_unconditional_branch(&merge_block);
        let else_block = builder.get_insert_block().unwrap();
        builder.position_at_end(&merge_block);
        let phi_node = builder.build_phi(ty_info.look_up(&self.ty_id).to_llvm_basic_type(), "");
        phi_node.add_incoming(&[(t_value, &then_block), (f_value, &else_block)]);
        phi_node
    }
}

impl mir::CallMir {
    fn code_gen(
        self,
        module: &module::Module,
        builder: &builder::Builder,
        params: &Vec<values::BasicValueEnum>,
        ty_info: &mut TypeInfo,
        function: values::FunctionValue,
    ) -> values::BasicValueEnum {
        let mut params_val: Vec<_> = self
            .params
            .into_iter()
            .map(|x| x.code_gen(module, builder, params, ty_info, function).as_basic_value_enum())
            .collect();
        match self.func.get_ty(ty_info) {
            Type::LambdaType(x) => {
                let callsite =
                    if let Some(env_ty) = x.env_ty {
                        let lambda = self.func.code_gen(module, builder, params, ty_info, function);
                        let lambda_ptr = builder.build_alloca(lambda.as_basic_value_enum().get_type(), "");
                        builder.build_store(lambda_ptr, lambda);
                        let envs_tuple_pointer = unsafe { builder.build_struct_gep(lambda_ptr, 0, "") };
                        let func_pointer =
                            builder.build_load(unsafe { builder.build_struct_gep(lambda_ptr, 1, "") }, "").into_pointer_value();
                        let mut envs_val: Vec<_> = env_ty
                            .element_tys
                            .into_iter()
                            .enumerate()
                            .map(|(idx, _)| {
                                let pointer =
                                    unsafe { builder.build_struct_gep(envs_tuple_pointer, idx as u32, "") };
                                builder.build_load(pointer, "")
                            }).collect();
                        envs_val.append(&mut params_val);
                        builder.build_call_pointer(func_pointer, &envs_val, "")
                    } else {
                        let func_pointer = self.func.code_gen(module, builder, params, ty_info, function).into_pointer_value();

                        builder.build_call_pointer(func_pointer, &params_val, "")
                    };
                callsite.try_as_basic_value().left().unwrap()
            }
            x => panic!("{:?}", x),
        }
    }
}

impl mir::TupleMir {
    fn code_gen(
        self,
        module: &module::Module,
        builder: &builder::Builder,
        params: &Vec<values::BasicValueEnum>,
        ty_info: &mut TypeInfo,
        function: values::FunctionValue,
    ) -> values::BasicValueEnum {
        let elements_val: Vec<_> = self
            .elements
            .into_iter()
            .map(|x| x.code_gen(module, builder, params, ty_info, function))
            .collect();
        let ty = types::StructType::struct_type(&elements_val.iter().map(|x| x.get_type()).collect::<Vec<_>>(), true);
        let val = builder.build_alloca(ty, "");
        elements_val.into_iter().enumerate().for_each(|(id, x)| {
            let ptr = unsafe { builder.build_struct_gep(val, id as u32, "") };
            builder.build_store(ptr, x);
        });
        builder.build_load(val, "ret")
    }
}

impl mir::OpMir {
    fn code_gen(
        self,
        module: &module::Module,
        builder: &builder::Builder,
        params: &Vec<values::BasicValueEnum>,
        ty_info: &mut TypeInfo,
        function: values::FunctionValue,
    ) -> values::BasicValueEnum {
        let lhs = self.l_expr.code_gen(module, builder, params, ty_info, function).into_int_value();
        let rhs = self.r_expr.code_gen(module, builder, params, ty_info, function).into_int_value();
        match &self.op as &str {
            "+" => builder.build_int_add::<values::IntValue>(lhs, rhs, ""),
            "-" => builder.build_int_sub(lhs, rhs, ""),
            "*" => builder.build_int_mul(lhs, rhs, ""),
            "==" => builder.build_int_compare(IntPredicate::EQ, lhs, rhs, ""),
            "/" => {
                let lhs = builder.build_signed_int_to_float(lhs, types::FloatType::f64_type(), "");
                let rhs = builder.build_signed_int_to_float(rhs, types::FloatType::f64_type(), "");
                builder.build_float_to_signed_int(builder.build_float_div(lhs, rhs, ""), types::IntType::i32_type(), "")
            }
            _ => panic!("error"),
        }.as_basic_value_enum()
    }
}

impl mir::LambdaMir {
    fn code_gen(
        self,
        module: &module::Module,
        builder: &builder::Builder,
        params: &Vec<values::BasicValueEnum>,
        ty_info: &mut TypeInfo,
    ) -> values::BasicValueEnum {
        //ラムダ式の関数作成
        let func_ty = ty_info
            .look_up_func_name(self.func_name.clone())
            .to_llvm_any_type(false).into_function_type();
        let func = module.get_function(&self.func_name).unwrap();

        if self.env.len() == 0 {
            func.as_any_value_enum().as_pointer_value().as_basic_value_enum()
        } else {
            //環境の値取得
            let env_val: Vec<_> = self
                .env
                .into_iter()
                .map(|x| params[params.len() - x.id - 1])
                .collect();
            //環境の型生成
            let env_llvm_ty =
                types::StructType::struct_type(&env_val.iter().map(|x| x.get_type()).collect::<Vec<_>>(), true);

            //ラムダ式の型生成
            let lambda_ty = types::StructType::struct_type(&[env_llvm_ty.as_basic_type_enum(), func_ty.ptr_type(AddressSpace::Generic).as_basic_type_enum()], true);

            //ラムダ式の生成
            let env_tuple_val = builder.build_alloca(env_llvm_ty, "");
            env_val.into_iter().enumerate().for_each(|(id, x)| {
                let ptr = unsafe { builder.build_struct_gep(env_tuple_val, id as u32, "") };
                builder.build_store(ptr, x);
            });
            let env_tuple_val = builder.build_load(env_tuple_val, "");
            let lambda_val = builder.build_alloca(lambda_ty, "");
            let env_tuple_ptr = unsafe { builder.build_struct_gep(lambda_val, 0, "") };
            builder.build_store(env_tuple_ptr, env_tuple_val);
            let func_ptr = unsafe { builder.build_struct_gep(lambda_val, 1, "") };
            builder.build_store(func_ptr, func.as_any_value_enum().into_pointer_value().as_basic_value_enum());
            builder.build_load(lambda_val, "ret")
        }
    }
}

impl mir::IndexPropertyMir {
    fn code_gen(
        self,
        module: &module::Module,
        builder: &builder::Builder,
        params: &Vec<values::BasicValueEnum>,
        ty_info: &mut TypeInfo,
        function: values::FunctionValue,
    ) -> values::BasicValueEnum {
        let expr_ty = self.expr.get_ty(ty_info);
        let expr_ptr = builder.build_alloca(expr_ty.to_llvm_basic_type(), "");
        let expr_value = self.expr.code_gen(module, builder, params, ty_info, function);
        builder.build_store(expr_ptr, expr_value);
        let ptr = unsafe {
            builder.build_struct_gep(
                expr_ptr,
                self.index,
                "",
            )
        };
        builder.build_load(ptr, "")
    }
}


impl mir::NamePropertyMir {
    fn code_gen(
        self,
        module: &module::Module,
        builder: &builder::Builder,
        params: &Vec<values::BasicValueEnum>,
        ty_info: &mut TypeInfo,
        function: values::FunctionValue,
    ) -> values::BasicValueEnum {
        let expr_ty = self.expr.get_ty(ty_info);
        let index = match expr_ty {
            Type::StructType(ref x) => {
                match x.ty {
                    StructInternalType::RecordType(ref x) => {
                        x.element_tys
                            .iter().enumerate()
                            .find(|(_, (name, _))| &self.property_name == name)
                            .unwrap()
                            .0
                    }
                    _ => panic!("bug!")
                }
            }
            _ => panic!("bug!")
        };
        let expr_ptr = builder.build_alloca(expr_ty.to_llvm_basic_type(), "");
        let expr_value = self.expr.code_gen(module, builder, params, ty_info, function);
        builder.build_store(expr_ptr, expr_value);
        let ptr = unsafe {
            builder.build_struct_gep(
                expr_ptr,
                index as u32,
                "",
            )
        };
        builder.build_load(ptr, "")
    }
}