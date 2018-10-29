use super::super::my_llvm::easy::*;
use super::semantic_analysis::ir_tree as ir;
use super::types::*;
use super::semantic_analysis::type_env::TypeResolved;

pub struct CodeGenResult<'a> {
    pub file_name: &'a str,
    pub code_gen: CodeGenerator,
    pub module: Module,
}

//コード生成する関数
impl ir::ProgramIr {
    pub fn code_gen(self, file_name: &str, ty_resolved: TypeResolved) -> CodeGenResult {
        //llvm初期化
        init_llvm_all_target();
        let code_gen = CodeGenerator::new();
        let module = Module::new(file_name);

        //外部関数宣言のコード化
        self.ex_dec_func_list
            .into_iter()
            .for_each(|(_, v)| ex_func_gen(v, &module, &ty_resolved));

        //関数宣言のコード化
        self.func_list.keys().for_each(|k| {
            let func_type = ty_resolved.get(k.clone()).to_llvm_type(false);
            let func = Function::new(&k, &module, func_type);
            set_linkage(func.llvm_function, LLVMLinkage::LLVMExternalLinkage);
        });

        //関数定義のコード化
        self.func_list
            .into_iter()
            .for_each(|(_, func)| func.code_gen(&module, &code_gen, &ty_resolved));

        module.dump_module();
        if let Some(err_msg) = module.verify_module() {
            panic!("llvm error:{}", err_msg);
        }

        CodeGenResult {
            file_name,
            module,
            code_gen,
        }
    }
}

fn ex_func_gen(dec_func_ir: ir::DecFuncIr, module: &Module, ty_resolved: &TypeResolved) {
    match ty_resolved.get(dec_func_ir.name.clone()) {
        Type::FuncType(ty) => {
            let function = Function::new(
                &dec_func_ir.name,
                &module,
                Type::FuncType(Box::new(*ty)).to_llvm_type(false),
            );
            set_linkage(function.llvm_function, LLVMLinkage::LLVMExternalLinkage);
        }
        _ => panic!("error!")
    }
}

impl Type {
    fn to_llvm_type(&self, fn_pointer_flag: bool) -> LLVMTypeRef {
        match self {
            Type::Int32 => int32_type(),
            Type::FuncType(x) =>
                if fn_pointer_flag {
                    pointer_type(x.to_llvm_type())
                } else {
                    x.to_llvm_type()
                },
            Type::TupleType(x) => x.to_llvm_type(),
            Type::TyVar(_) => panic!("TyVar type!"),
            Type::LambdaType(x) => x.to_llvm_type()
        }
    }
}

impl FuncType {
    fn to_llvm_type(&self) -> LLVMTypeRef {
        function_type(
            self.ret_type.to_llvm_type(true),
            self.param_types.iter()
                .map(|x| x.to_llvm_type(true))
                .collect(),
        )
    }
}

impl TupleType {
    fn to_llvm_type(&self) -> LLVMTypeRef {
        struct_type(
            self.element_tys.iter().map(|x| x.to_llvm_type(true)).collect(),
        )
    }
}

impl LambdaType {
    fn to_llvm_type(&self) -> LLVMTypeRef {
        let envs_llvm_ty =
            self.env_tys.iter().map(|x| x.to_llvm_type(true)).collect();
        let func_llvm_ty = pointer_type( self.func_ty.to_llvm_type());
        struct_type(vec![struct_type(envs_llvm_ty), func_llvm_ty])
    }
}

impl ir::FuncIr {
    fn code_gen(self, module: &Module, codegen: &CodeGenerator, ty_resolved: &TypeResolved) {
        let function = module.get_named_function(&self.name);
        let params = function.get_params(self.params_len);
        let entry_block = function.append_basic_block(&("entry_".to_string()+&self.name));
        codegen.position_builder_at_end(entry_block);
        let value = self.body.code_gen(&module, &codegen, &params, ty_resolved);
        codegen.position_builder_at_end(entry_block);
        codegen.build_ret(value);
    }
}

impl ir::ExprIr {
    fn code_gen(
        self,
        module: &Module,
        codegen: &CodeGenerator,
        params: &Vec<LLVMValueRef>,
        ty_resolved: &TypeResolved,
    ) -> LLVMValueRef {
        match self {
            ir::ExprIr::NumIr(num_ir) => const_int(int32_type(), num_ir.num as u64, true),
            ir::ExprIr::OpIr(op_ir) => op_ir.code_gen(module, codegen, params, ty_resolved),
            ir::ExprIr::VariableIr(var_ir) => params[params.len() - var_ir.id - 1],
            ir::ExprIr::GlobalVariableIr(x) => x.code_gen(module),
            ir::ExprIr::CallIr(x) => x.code_gen(module, codegen, params, ty_resolved),
            ir::ExprIr::TupleIr(x) => x.code_gen(module, codegen, params, ty_resolved),
            ir::ExprIr::LambdaIr(x) => x.code_gen(module, codegen, params, ty_resolved)
        }
    }
}

impl ir::GlobalVariableIr {
    fn code_gen(self, module: &Module) -> LLVMValueRef {
        let func = module.get_named_function(&self.id);
        func.llvm_function
    }
}

impl ir::CallIr {
    fn code_gen(
        self,
        module: &Module,
        codegen: &CodeGenerator,
        params: &Vec<LLVMValueRef>,
        ty_resolved: &TypeResolved,
    ) -> LLVMValueRef {
        let func = self.func.code_gen(module, codegen, params, ty_resolved);
        let params = self
            .params
            .into_iter()
            .map(|x| x.code_gen(module, codegen, params, ty_resolved))
            .collect();

        codegen.build_call(func, params, "")
    }
}

impl ir::TupleIr {
    fn code_gen(
        self,
        module: &Module,
        codegen: &CodeGenerator,
        params: &Vec<LLVMValueRef>,
        ty_resolved: &TypeResolved,
    ) -> LLVMValueRef {
        let elements_val: Vec<LLVMValueRef> =
            self.elements
                .into_iter()
                .map(|x| x.code_gen(module, codegen, params, ty_resolved)).collect();
        let ty = struct_type(
            elements_val.iter()
                .map(|x| type_of(*x))
                .collect()
        );
        let val = codegen.build_alloca(ty, "");
        elements_val
            .into_iter()
            .enumerate()
            .for_each(|(id, x)| {
                let ptr =
                    codegen.build_struct_gep(val, id as u32, "");
                codegen.build_store(x, ptr);
            });
        codegen.build_load(val, "ret")
    }
}

impl ir::OpIr {
    fn code_gen(
        self,
        module: &Module,
        codegen: &CodeGenerator,
        params: &Vec<LLVMValueRef>,
        ty_resolved: &TypeResolved,
    ) -> LLVMValueRef {
        let lhs = self.l_expr.code_gen(module, codegen, params, ty_resolved);
        let rhs = self.r_expr.code_gen(module, codegen, params, ty_resolved);
        match &self.op as &str {
            "+" => codegen.build_add(lhs, rhs, ""),
            "-" => codegen.build_sub(lhs, rhs, ""),
            "*" => codegen.build_mul(lhs, rhs, ""),
            "/" => {
                let lhs = codegen.build_si_to_fp(lhs, double_type(), "");
                let rhs = codegen.build_si_to_fp(rhs, double_type(), "");
                codegen.build_fp_to_si(codegen.build_fdiv(lhs, rhs, ""), int32_type(), "")
            }
            _ => panic!("error"),
        }
    }
}

impl ir::LambdaIr {
    fn code_gen(
        self,
        module: &Module,
        codegen: &CodeGenerator,
        params: &Vec<LLVMValueRef>,
        ty_resolved: &TypeResolved,
    ) -> LLVMValueRef {
        //環境の値取得
        let env_val: Vec<LLVMValueRef> = self.env.into_iter()
            .map(|x| params[params.len() - x.id - 1])
            .collect();
        //環境の型生成
        let env_llvm_ty = struct_type(
            env_val.iter().map(|x| type_of(*x)).collect()
        );

        //ラムダ式の関数作成
        let func_ty = ty_resolved.get(self.func.name.clone()).to_llvm_type(false);
        let func = Function::new(&self.func.name, &module, func_ty);
        set_linkage(func.llvm_function, LLVMLinkage::LLVMExternalLinkage);

        //ラムダ式の型生成
        let lambda_ty = struct_type(vec![env_llvm_ty, pointer_type(func_ty)]);

        //ラムダ式の生成
        let env_tuple_val = codegen.build_alloca(env_llvm_ty, "");
        let func_val =
            env_val
                .into_iter()
                .enumerate()
                .for_each(|(id, x)| {
                    let ptr =
                        codegen.build_struct_gep(env_tuple_val, id as u32, "");
                    codegen.build_store(x, ptr);
                });
        let env_tuple_val=codegen.build_load(env_tuple_val,"");
        let lambda_val = codegen.build_alloca(lambda_ty, "");
        let env_tuple_ptr = codegen.build_struct_gep(lambda_val, 0, "");
        codegen.build_store(env_tuple_val, env_tuple_ptr);
        let func_ptr = codegen.build_struct_gep(lambda_val, 1, "");
        codegen.build_store(func.llvm_function, func_ptr);
        let ret=codegen.build_load(lambda_val, "ret");

        //ラムダ式の中身を生成
        self.func.code_gen(module, codegen, ty_resolved);
        ret
    }
}
