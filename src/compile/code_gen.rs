extern crate inkwell;

use self::inkwell::*;
use self::inkwell::{values::BasicValue, values::AnyValue, types::BasicType, types::AnyType};
use super::ir::mir;
use super::semantic_analysis::type_inference::type_substitute::TypeSubstitute;
use super::semantic_analysis::type_inference::assump_env::AssumpEnv;
use super::types::types::*;
use std::collections::hash_map::HashMap;
use compile::mangling::mangle;

pub struct CodeGenResult<'a> {
    pub file_name: &'a str,
    pub builder: builder::Builder,
    pub module: module::Module,
}

type FuncList = HashMap<String, mir::FuncMir>;

struct GenInfo<'a> {
    pub module: &'a module::Module,
    pub builder: &'a builder::Builder,
    pub params: Vec<values::BasicValueEnum>,
    pub params_ty: &'a Vec<Type>,
    pub ty_sub: &'a mut TypeSubstitute,
    pub function: values::FunctionValue,
    pub func_list: &'a FuncList,
    pub assump: &'a AssumpEnv,
}

//コード生成する関数
impl mir::ProgramMir {
    pub fn code_gen(self, file_name: &str, assump: AssumpEnv) -> CodeGenResult {
        //llvm初期化
        targets::Target::initialize_all(&targets::InitializationConfig::default());
        let builder = builder::Builder::create();
        let module = module::Module::create(file_name);

        let mut ty_sub = self.ty_sub;
        //外部関数宣言のコード化
        self.ex_dec_func_list
            .into_iter()
            .for_each(|x| ex_func_gen(x, &module, &mut ty_sub));

        let main_func = match self.implicit_func_list.get("main") {
            None => &self.explicit_func_list.iter().find(|x| x.func.name == "main").unwrap().func,
            Some(func) => &func.func
        }.clone();

        //関数定義のコード化
        let func_list = self.implicit_func_list
            .into_iter().map(|(_, x)| x.func)
            .chain(self.explicit_func_list.into_iter().map(|x| x.func))
            .map(|func| (func.name.clone(), func))
            .collect::<FuncList>();

        let main_func_ty = Type::create_func_type(vec![Type::create_tuple_type(vec![])], Type::create_int32());
        main_func.code_gen(&module, &builder, &mut ty_sub, &main_func_ty, &func_list, &assump);
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

//関数を取得する。存在しない場合は新たに登録する。
fn get_function(name: &String, ty: &Type, module: &module::Module, builder: &builder::Builder, ty_sub: &mut TypeSubstitute, func_list: &FuncList, assump: &AssumpEnv) -> values::FunctionValue {
    use super::types::Qual;
    match module.get_function(&name) {
        Some(func) => func,
        None => if name == "main" {
            add_function(name, &ty, module, true)
        } else {
            let ty = ty_sub.qual_unify(assump.global_get(&name).unwrap().get_qual().clone(), Qual::new(ty.clone())).unwrap().t;
            let ty = ty_sub.type_look_up(&ty, true);
            match module.get_function(&mangle(name, &ty)) {
                Some(func) => func,
                None => {
                    let func = add_function(name, &ty, module, false);
                    let hoge = builder.get_insert_block().unwrap();
                    func_list[name].clone().code_gen(module, builder, ty_sub, &ty, func_list, assump);
                    builder.position_at_end(&hoge);
                    func
                }
            }
        }
    }
}

fn add_function(name: &String, ty: &Type, module: &module::Module, no_mangle: bool) -> values::FunctionValue {
    let mangled_name = if no_mangle { name.to_string() } else { mangle(name, ty) };
    module.add_function(&mangled_name, ty.to_llvm_any_type(false).as_any_type_enum().into_function_type(), Some(module::Linkage::External))
}

fn ex_func_gen(dec_func_ir: mir::DecFuncMir, module: &module::Module, ty_sub: &mut TypeSubstitute) {
    use compile::types::show_type::ShowType;
    let func_id_type = ty_sub.ty_env.global_get(dec_func_ir.name.clone());
    match ty_sub.type_look_up(&func_id_type, false) {
        Type::LambdaType(ty) =>
            add_function(&dec_func_ir.name, &Type::LambdaType(ty.clone()), module, true),
        x => panic!("error!{}", x.show()),
    };
}

impl mir::ExprMir {
    fn get_ty(&self, ty_sub: &mut TypeSubstitute, params_ty: &Vec<Type>) -> Type {
        match self {
            mir::ExprMir::NumMir(_) => Type::create_int32(),
            mir::ExprMir::BoolMir(_) => Type::create_bool(),
            mir::ExprMir::OpMir(_) => Type::create_int32(),
            mir::ExprMir::VariableMir(x) => params_ty[params_ty.len() - x.id - 1].clone(),
            mir::ExprMir::IfMir(x) => ty_sub.look_up(&x.ty_id, false),
            mir::ExprMir::GlobalVariableMir(x) => ty_sub.look_up(&x.ty_id, false),
            mir::ExprMir::CallMir(x) => ty_sub.look_up(&x.ty_id, false),
            mir::ExprMir::TupleMir(x) => ty_sub.look_up(&x.ty_id, false),
            mir::ExprMir::TupleStructMir(_) => panic!("undefined"),
            mir::ExprMir::IndexPropertyMir(x) => ty_sub.look_up(&x.ty_id, false),
            mir::ExprMir::NamePropertyMir(x) => ty_sub.look_up(&x.ty_id, false),
            mir::ExprMir::LambdaMir(x) => ty_sub.look_up(&x.ty_id, false)
        }
    }
}

impl mir::FuncMir {
    fn code_gen(self, module: &module::Module, builder: &builder::Builder, ty_sub: &mut TypeSubstitute, ty: &Type, func_list: &FuncList, assump: &AssumpEnv) {
        let mut ty_sub = ty_sub.clone();
        let function = get_function(&self.name, &ty, module, builder, &mut ty_sub, func_list, assump);
        let ty = ty_sub.type_look_up(ty, true);
        let params = function.get_params();
        let entry_block = function.append_basic_block(&"entry");
        builder.position_at_end(&entry_block);
        let mut gen_info = GenInfo { module, builder, params, ty_sub: &mut ty_sub, function, func_list, params_ty: &ty.get_lambda_ty().func_ty.param_types, assump };
        let value = self.body.code_gen(&mut gen_info);
        builder.build_return(Some(&value));
    }
}

impl mir::ExprMir {
    fn code_gen(
        self,
        gen_info: &mut GenInfo,
    ) -> values::BasicValueEnum {
        match self {
            mir::ExprMir::NumMir(num_ir) => types::IntType::i32_type().const_int(num_ir.num as u64, true).as_basic_value_enum(),
            mir::ExprMir::BoolMir(bool_ir) => types::IntType::bool_type().const_int(
                if bool_ir.bool { 1 } else { 0 },
                false,
            ).as_basic_value_enum(),
            mir::ExprMir::IfMir(x) => x.code_gen(gen_info).as_basic_value(),
            mir::ExprMir::OpMir(op_ir) => op_ir.code_gen(gen_info),
            mir::ExprMir::VariableMir(var_ir) => gen_info.params[gen_info.params.len() - var_ir.id - 1],
            mir::ExprMir::GlobalVariableMir(x) => x.code_gen(gen_info).as_any_value_enum().into_pointer_value().as_basic_value_enum(),
            mir::ExprMir::TupleMir(x) => x.code_gen(gen_info),
            mir::ExprMir::TupleStructMir(x) => x.tuple.code_gen(gen_info),
            mir::ExprMir::LambdaMir(x) => x.code_gen(gen_info),
            mir::ExprMir::CallMir(x) => x.code_gen(gen_info),
            mir::ExprMir::IndexPropertyMir(x) => x.code_gen(gen_info),
            mir::ExprMir::NamePropertyMir(x) => x.code_gen(gen_info),
        }
    }
}

impl mir::GlobalVariableMir {
    fn code_gen(self, gen_info: &mut GenInfo) -> values::FunctionValue {
        let func_ty = gen_info.ty_sub.look_up(&self.ty_id, false);
        get_function(&self.id, &func_ty, gen_info.module, gen_info.builder, &mut gen_info.ty_sub.clone(), gen_info.func_list, gen_info.assump)
    }
}

impl mir::IfMir {
    fn code_gen(
        self,
        gen_info: &mut GenInfo,
    ) -> values::PhiValue {
        let cond_value = self.cond.code_gen(gen_info).into_int_value();
        let then_block = gen_info.function.append_basic_block(&"then");
        let else_block = gen_info.function.append_basic_block(&"else");
        let merge_block = gen_info.function.append_basic_block(&"merge");
        gen_info.builder.build_conditional_branch(cond_value, &then_block, &else_block);
        gen_info.builder.position_at_end(&then_block);
        let t_value: &values::BasicValue = &self.t_expr.code_gen(gen_info);
        gen_info.builder.build_unconditional_branch(&merge_block);
        let then_block = gen_info.builder.get_insert_block().unwrap();
        gen_info.builder.position_at_end(&else_block);
        let f_value: &values::BasicValue = &self.f_expr.code_gen(gen_info);
        gen_info.builder.build_unconditional_branch(&merge_block);
        let else_block = gen_info.builder.get_insert_block().unwrap();
        gen_info.builder.position_at_end(&merge_block);
        let phi_node = gen_info.builder.build_phi(gen_info.ty_sub.look_up(&self.ty_id, false).to_llvm_basic_type(), "");
        phi_node.add_incoming(&[(t_value, &then_block), (f_value, &else_block)]);
        phi_node
    }
}

impl mir::CallMir {
    fn code_gen(
        self,
        gen_info: &mut GenInfo,
    ) -> values::BasicValueEnum {
        let mut params_val: Vec<_> = self
            .params
            .into_iter()
            .map(|x| x.code_gen(gen_info).as_basic_value_enum())
            .collect();
        match self.func.get_ty(gen_info.ty_sub, gen_info.params_ty) {
            Type::LambdaType(x) => {
                let callsite =
                    if let Some(env_ty) = x.env_ty {
                        let lambda = self.func.code_gen(gen_info);
                        println!("{}", format!("{:?}", lambda.get_type()));
                        let lambda_ptr = gen_info.builder.build_alloca(lambda.as_basic_value_enum().get_type(), "");
                        gen_info.builder.build_store(lambda_ptr, lambda);
                        let envs_tuple_pointer = unsafe { gen_info.builder.build_struct_gep(lambda_ptr, 0, "") };
                        let func_pointer =
                            gen_info.builder.build_load(unsafe { gen_info.builder.build_struct_gep(lambda_ptr, 1, "") }, "").into_pointer_value();
                        let mut envs_val: Vec<_> = env_ty
                            .element_tys
                            .into_iter()
                            .enumerate()
                            .map(|(idx, _)| {
                                let pointer =
                                    unsafe { gen_info.builder.build_struct_gep(envs_tuple_pointer, idx as u32, "") };
                                gen_info.builder.build_load(pointer, "")
                            }).collect();
                        envs_val.append(&mut params_val);
                        gen_info.builder.build_call_pointer(func_pointer, &envs_val, "")
                    } else {
                        let func_pointer = self.func.code_gen(gen_info).into_pointer_value();

                        gen_info.builder.build_call_pointer(func_pointer, &params_val, "")
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
        gen_info: &mut GenInfo,
    ) -> values::BasicValueEnum {
        let elements_val: Vec<_> = self
            .elements
            .into_iter()
            .map(|x| x.code_gen(gen_info))
            .collect();
        let ty = types::StructType::struct_type(&elements_val.iter().map(|x| x.get_type()).collect::<Vec<_>>(), true);
        let val = gen_info.builder.build_alloca(ty, "");
        elements_val.into_iter().enumerate().for_each(|(id, x)| {
            let ptr = unsafe { gen_info.builder.build_struct_gep(val, id as u32, "") };
            gen_info.builder.build_store(ptr, x);
        });
        gen_info.builder.build_load(val, "ret")
    }
}

impl mir::OpMir {
    fn code_gen(
        self,
        gen_info: &mut GenInfo,
    ) -> values::BasicValueEnum {
        let lhs = self.l_expr.code_gen(gen_info).into_int_value();
        let rhs = self.r_expr.code_gen(gen_info).into_int_value();
        match &self.op as &str {
            "+" => gen_info.builder.build_int_add::<values::IntValue>(lhs, rhs, ""),
            "-" => gen_info.builder.build_int_sub(lhs, rhs, ""),
            "*" => gen_info.builder.build_int_mul(lhs, rhs, ""),
            "==" => gen_info.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, ""),
            "/" => {
                let lhs = gen_info.builder.build_signed_int_to_float(lhs, types::FloatType::f64_type(), "");
                let rhs = gen_info.builder.build_signed_int_to_float(rhs, types::FloatType::f64_type(), "");
                gen_info.builder.build_float_to_signed_int(gen_info.builder.build_float_div(lhs, rhs, ""), types::IntType::i32_type(), "")
            }
            _ => panic!("error"),
        }.as_basic_value_enum()
    }
}

impl mir::LambdaMir {
    fn code_gen(
        self,
        gen_info: &mut GenInfo,
    ) -> values::BasicValueEnum {
        //ラムダ式の関数作成
        let func_ty = gen_info.ty_sub.look_up(&self.func_id, false);
        let func = get_function(&self.func_name, &func_ty, gen_info.module, gen_info.builder, &mut gen_info.ty_sub.clone(), gen_info.func_list, gen_info.assump);
        let func_llvm_ty = func_ty
            .to_llvm_any_type(false).into_function_type();

        if self.env.len() == 0 {
            func.as_any_value_enum().as_pointer_value().as_basic_value_enum()
        } else {
            //環境の値取得
            let env_val: Vec<_> = self
                .env
                .into_iter()
                .map(|x| gen_info.params[gen_info.params.len() - x.id - 1])
                .collect();
            //環境の型生成
            let env_llvm_ty =
                types::StructType::struct_type(&env_val.iter().map(|x| x.get_type()).collect::<Vec<_>>(), true);

            //ラムダ式の型生成
            let lambda_ty = types::StructType::struct_type(&[env_llvm_ty.as_basic_type_enum(), func_llvm_ty.ptr_type(AddressSpace::Generic).as_basic_type_enum()], true);

            //ラムダ式の生成
            let env_tuple_val = gen_info.builder.build_alloca(env_llvm_ty, "");
            env_val.into_iter().enumerate().for_each(|(id, x)| {
                let ptr = unsafe { gen_info.builder.build_struct_gep(env_tuple_val, id as u32, "") };
                gen_info.builder.build_store(ptr, x);
            });
            let env_tuple_val = gen_info.builder.build_load(env_tuple_val, "");
            let lambda_val = gen_info.builder.build_alloca(lambda_ty, "");
            let env_tuple_ptr = unsafe { gen_info.builder.build_struct_gep(lambda_val, 0, "") };
            gen_info.builder.build_store(env_tuple_ptr, env_tuple_val);
            let func_ptr = unsafe { gen_info.builder.build_struct_gep(lambda_val, 1, "") };
            gen_info.builder.build_store(func_ptr, func.as_any_value_enum().into_pointer_value().as_basic_value_enum());
            gen_info.builder.build_load(lambda_val, "ret")
        }
    }
}

impl mir::IndexPropertyMir {
    fn code_gen(
        self,
        gen_info: &mut GenInfo,
    ) -> values::BasicValueEnum {
        let expr_value = self.expr.clone().code_gen(gen_info);
        let expr_ty = expr_value.get_type();//self.expr.get_ty(gen_info.ty_sub, gen_info.params_ty);
        let expr_ptr = gen_info.builder.build_alloca(expr_ty, "");
        gen_info.builder.build_store(expr_ptr, expr_value);
        let ptr = unsafe {
            gen_info.builder.build_struct_gep(
                expr_ptr,
                self.index,
                "",
            )
        };
        gen_info.builder.build_load(ptr, "")
    }
}


impl mir::NamePropertyMir {
    fn code_gen(
        self,
        gen_info: &mut GenInfo,
    ) -> values::BasicValueEnum {
        let expr_ty = self.expr.get_ty(gen_info.ty_sub, gen_info.params_ty);
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
        let expr_ptr = gen_info.builder.build_alloca(expr_ty.to_llvm_basic_type(), "");
        let expr_value = self.expr.code_gen(gen_info);
        gen_info.builder.build_store(expr_ptr, expr_value);
        let ptr = unsafe {
            gen_info.builder.build_struct_gep(
                expr_ptr,
                index as u32,
                "",
            )
        };
        gen_info.builder.build_load(ptr, "")
    }
}