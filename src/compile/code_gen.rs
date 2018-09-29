use super::super::my_llvm::easy::*;
use super::semantic_analysis::ir_tree as ir;
use super::types;

fn extern_func_gen(dec_func_ir: ir::DecFuncIr,module:&Module)  {
    let function_type=dec_func_ir.ty.to_llvm_type();
    let function = Function::new(&dec_func_ir.name, &module, function_type);
    set_linkage(function.llvm_function, LLVMLinkage::LLVMExternalLinkage);
}

//コード生成する関数
impl ir::ProgramIr {
    pub fn code_gen(self, file_name: &str) {
        //llvm初期化
        init_llvm_all_target();
        let codegen = CodeGenerator::new();
        let module = Module::new("my_module");

        //外部関数宣言のコード化
        self.extern_func_list.into_iter()
            .for_each(|(_,v)|extern_func_gen(v,&module));

        //関数宣言のコード化
        self.func_list.iter().for_each(|(k, v)| {
            let func_type = v.ty.to_llvm_type();
            let func = Function::new(&k, &module, func_type);
            set_linkage(func.llvm_function, LLVMLinkage::LLVMExternalLinkage);
        });

        //関数定義のコード化
        self.func_list
            .into_iter()
            .for_each(|(_, func)| func.code_gen(&module, &codegen));

        module.dump_module();
        if let Some(err_msg) = module.verify_module() {
            panic!("llvm error:{}", err_msg);
        }

        output_file(file_name, module, codegen);
    }
}

impl types::Type {
    fn to_llvm_type(&self) -> LLVMTypeRef {
        match self {
            types::Type::Int32 => int32_type(),
            types::Type::FuncType(x) => x.to_llvm_type(),
            types::Type::Unknown => panic!("unknown type!"),
            types::Type::Fn(x) =>
                match &**x {
                    types::Type::FuncType(y) => pointer_type(y.to_llvm_type()),
                    _ => panic!("Fn error type!")
                }
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

impl ir::FuncIr {
    fn code_gen(self, module: &Module, codegen: &CodeGenerator) {
        let function = module.get_named_function(&self.name);
        let params = function.get_params(self.params.len());
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
            .map(|x| {
                x.code_gen(module, codegen, params)
            })
            .collect();

        codegen.build_call(func, params, "")
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

//コードを実行形式で出力
fn output_file(file_name: &str, module: Module, codegen: CodeGenerator) {
    use std::env;
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
        command_exec(
            "cmd",
            &[
                "/C",
                &(current_dir.clone() + "\\compile.bat"),
                &(current_dir + "\\" + file_name + ".obj"),
            ],
        );
    } else {
        command_exec(
            "sh",
            &[
                "-c",
                &("g++ ".to_owned()
                    + &(current_dir.clone() + "/" + file_name + ".obj ")
                    + &(current_dir.clone() + "/" + "libtest.a ")
                    + "-o "
                    + &(current_dir + "/" + file_name + ".out")),
            ],
        );
    };
}

use std::ffi::OsStr;

fn command_exec<I, S>(terminal: &str, args: I)
    where
        I: IntoIterator<Item=S> + Clone,
        S: AsRef<OsStr>,
{
    use std::process::Command;

    let output = Command::new(terminal)
        .args(args.clone())
        .output()
        .expect("failed to execute process");
    println!("status: {}", output.status);
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    println!(
        "command: {}",
        args.into_iter()
            .fold("".to_string(), |acc, x| acc.to_owned()
                + " "
                + x.as_ref().to_str().unwrap())
    );
}
