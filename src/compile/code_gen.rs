use super::super::my_llvm::easy::*;
use super::semantic_analysis::ir_tree as ir;
use super::types;

pub struct CodeGenResult<'a> {
    pub file_name: &'a str,
    pub code_gen: CodeGenerator,
    pub module: Module,
}

//コード生成する関数
impl ir::ProgramIr {
    pub fn code_gen(self, file_name: &str) -> CodeGenResult {
        //llvm初期化
        init_llvm_all_target();
        let code_gen = CodeGenerator::new();
        let module = Module::new(file_name);

        //外部関数宣言のコード化
        self.ex_dec_func_list
            .into_iter()
            .for_each(|(_, v)| ex_func_gen(v, &module));

        //関数宣言のコード化
        self.func_list.iter().for_each(|(k, v)| {
            let func_type = v.ty.to_llvm_type();
            let func = Function::new(&k, &module, func_type);
            set_linkage(func.llvm_function, LLVMLinkage::LLVMExternalLinkage);
        });

        //関数定義のコード化
        self.func_list
            .into_iter()
            .for_each(|(_, func)| func.code_gen(&module, &code_gen));

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

fn ex_func_gen(dec_func_ir: ir::DecFuncIr, module: &Module) {
    let function_type = dec_func_ir.ty.to_llvm_type();
    let function = Function::new(&dec_func_ir.name, &module, function_type);
    set_linkage(function.llvm_function, LLVMLinkage::LLVMExternalLinkage);
}

impl types::Type {
    fn to_llvm_type(&self) -> LLVMTypeRef {
        match self {
            types::Type::Int32 => int32_type(),
            types::Type::FuncType(x) => x.to_llvm_type(),
            types::Type::Unknown => panic!("unknown type!"),
            types::Type::Fn(x) => pointer_type(x.to_llvm_type()),
            types::Type::TupleType(x) => x.to_llvm_type()
        }
    }
}

impl types::FuncType {
    fn to_llvm_type(&self) -> LLVMTypeRef {
        function_type(
            self.ret_type.to_llvm_type(),
            self.param_types.iter().map(|x| x.to_llvm_type()).collect(),
        )
    }
}

impl types::TupleType {
    fn to_llvm_type(&self) -> LLVMTypeRef {
        struct_type(
            self.element_tys.iter().map(|x| x.to_llvm_type()).collect(),
        )
    }
}

impl ir::FuncIr {
    fn code_gen(self, module: &Module, codegen: &CodeGenerator) {
        let function = module.get_named_function(&self.name);
        let params = function.get_params(self.ty.param_types.len());
        let entry_block = function.append_basic_block("entry");
        codegen.position_builder_at_end(entry_block);
        let value = self.body.code_gen(&module, &codegen, &params);
        codegen.build_ret(value);
    }
}

impl ir::ExprIr {
    fn code_gen(
        self,
        module: &Module,
        codegen: &CodeGenerator,
        params: &Vec<LLVMValueRef>,
    ) -> LLVMValueRef {
        match self {
            ir::ExprIr::NumIr(num_ir) => const_int(int32_type(), num_ir.num as u64, true),
            ir::ExprIr::OpIr(op_ir) => op_ir.code_gen(module, codegen, params),
            ir::ExprIr::VariableIr(var_ir) => params[params.len() - var_ir.id - 1],
            ir::ExprIr::GlobalVariableIr(x) => x.code_gen(module),
            ir::ExprIr::CallIr(x) => x.code_gen(module, codegen, params),
            ir::ExprIr::TupleIr(x) => x.code_gen(module, codegen, params),
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
    ) -> LLVMValueRef {
        let func = self.func.code_gen(module, codegen, params);
        let params = self
            .params
            .into_iter()
            .map(|x| x.code_gen(module, codegen, params))
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
    ) -> LLVMValueRef {
        let val = codegen.build_alloca(self.ty.to_llvm_type(), "");
        self.elements
            .into_iter()
            .enumerate()
            .for_each(|(id,x)|{
                let x=x.code_gen(module, codegen, params);
                let ptr=
                    codegen.build_struct_gep(val,id as u32,"");
                codegen.build_store(x,ptr);
            });
        codegen.build_load(val,"ret")
    }
}

impl ir::OpIr {
    fn code_gen(
        self,
        module: &Module,
        codegen: &CodeGenerator,
        params: &Vec<LLVMValueRef>,
    ) -> LLVMValueRef {
        let lhs = self.l_expr.code_gen(module, codegen, params);
        let rhs = self.r_expr.code_gen(module, codegen, params);
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
