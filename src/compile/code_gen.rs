use super::super::my_llvm::easy::*;
use super::semantic_analysis::ir_tree as ir;

fn extern_test_func(module: &Module) -> Function {
    let function_type = function_type(void_type(), vec![int32_type()]);
    let function = Function::new("print", &module, function_type);
    set_linkage(function.llvm_function, LLVMLinkage::LLVMExternalLinkage);
    function
}

//コード生成する関数
impl ir::ProgramIr {
    pub fn code_gen(self, file_name: &str) {
        //llvm初期化
        init_llvm_all_target();
        let codegen = CodeGenerator::new();
        let module = Module::new("my_module");

        let print_func = extern_test_func(&module);

        self.func_list.into_iter().for_each(|func|
            func.code_gen(&print_func, &module, &codegen)
        );
        if let Some(err_msg) = module.verify_module() {
            panic!("llvm error:{}", err_msg);
        }
        module.dump_module();

        output_file(file_name, module, codegen);
    }
}

impl ir::FuncIr {
    fn code_gen(
        self,
        print_func: &Function,
        module: &Module,
        codegen: &CodeGenerator,
    ) {
        let function_type = function_type(
            int32_type(),
            self
                .params
                .iter()
                .map(|_| int32_type())
                .collect(),
        );
        let function = Function::new(&self.name, &module, function_type);
        let params = function.get_params(self.params.len());
        /*params.iter().
        zip(self.params.into_iter().rev()).
        for_each(|(val_ref, id)|
            set_value_name(*val_ref, &format!("{}", id)));*/

        let entry_block = function.append_basic_block("entry");
        codegen.position_builder_at_end(entry_block);
        let value = self.body.code_gen(&module, &codegen, &params);
        codegen.build_call(print_func.llvm_function, vec![value], "");
        codegen.build_ret(const_int(int32_type(), 0, false));
    }
}

impl ir::ExprIr {
    fn code_gen(self, module: &Module, codegen: &CodeGenerator, params: &Vec<LLVMValueRef>) -> LLVMValueRef {
        match self {
            ir::ExprIr::NumIr(num_ir) => const_int(int32_type(), num_ir.num as u64, true),
            ir::ExprIr::OpIr(op_ir) =>op_ir.code_gen(module, codegen, params),
            ir::ExprIr::VariableIr(var_ir) => params[params.len() - var_ir.id - 1],
        }
    }
}


impl ir::OpIr {
    fn code_gen(self, module: &Module, codegen: &CodeGenerator, params: &Vec<LLVMValueRef>) -> LLVMValueRef {
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

//コードをexeで出力
fn output_file(file_name: &str, module: Module, codegen: CodeGenerator) {
    use std::env;
    use std::process::Command;
    let target_machine = TargetMachine::create(
        "generic",
        "",
        LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
        LLVMRelocMode::LLVMRelocDefault,
        LLVMCodeModel::LLVMCodeModelDefault,
    ).unwrap();
    module.set_data_layout(target_machine.create_data_layout());
    module.set_target_triple(target_machine.target_triple);
    module.write_bitcode_to_file(&(file_name.to_string() + ".bc"));
    target_machine.emit_to_file(
        &module,
        &(file_name.to_string() + ".obj"),
        LLVMCodeGenFileType::LLVMObjectFile,
    );
    module.dispose_module();
    codegen.dispose();
    target_machine.dispose();

    let current_dir = env::current_dir().unwrap().to_str().unwrap().to_string();
    if cfg!(target_os = "windows") {
        Command::new("cmd")
            .args(&[
                "/C",
                &(current_dir.clone() + "\\compile.bat"),
                &(current_dir + "\\" + file_name + ".obj"),
            ])
            .output()
            .expect("failed to execute process")
    } else {
        panic!("support windows only!!")
    };
}
