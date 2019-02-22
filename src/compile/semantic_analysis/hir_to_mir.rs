use super::super::ir::ast::*;
use super::mir::*;
use super::super::ir::hir::*;
use super::variable_table::VariableTable;
use super::super::types::*;
use super::type_inference::type_env::TypeEnv;
use super::Error;
use super::type_variable_table::TypeVariableTable;
use std::collections::HashMap;

type AstToIrResult<T> = Result<T, Error>;

impl ProgramHir {
    //ASTをIRに変換
    pub fn to_mir(self) -> AstToIrResult<ProgramMir> {
        let mut lambda_count: usize = 0;
        let mut var_table = VariableTable::new(self.get_global_var_names());

        let mut program_mir = ProgramMir::empty();
        let struct_list = &self.struct_list;
        let mut dec_func_list = self.dec_func_list;
        program_mir = self.def_func_list
            .into_iter()
            .fold(Ok(program_mir), |acc, (_, x)| {
                x.to_mir(acc?, &mut dec_func_list, struct_list, &mut lambda_count, &mut var_table)
            })?;

        program_mir = self.ex_dec_func_list
            .into_iter()
            .fold(Ok(program_mir), |acc, (_, x)| {
                Ok(x.to_mir(acc?, struct_list))
            })?;
        Ok(program_mir)
    }
    //グローバル変数の名前一覧を取得（関数名しかないけど）
    fn get_global_var_names(&self) -> HashMap<String, ()> {
        self.def_func_list.iter()
            .map(|(_, x)| x.name.clone())
            .chain(
                self.dec_func_list.iter()
                    .chain(self.ex_dec_func_list.iter())
                    .map(|(_, x)| x.name.clone())
            )
            .map(|x| (x, ()))
            .collect()
    }
}


impl DefFuncAST {
    fn to_mir(
        self,
        mut program_ir: ProgramMir,
        dec_func_list: &mut HashMap<String, DecFuncHir>,
        struct_list: &HashMap<String, DecStructHir>,
        lambda_count: &mut usize,
        var_table: &mut VariableTable,
    ) -> AstToIrResult<ProgramMir> {
        let params_len: usize = if self.params.len() == 0 {
            1
        } else {
            self.params.len()
        };
        var_table.in_nest(self.params.into_iter().map(|x| x.id));
        let func_ir = FuncMir {
            name: self.name,
            body: self.body.to_mir(&mut program_ir, struct_list, var_table, lambda_count)?,
            params_len,
            pos: self.pos,
        };
        match dec_func_list.remove(&func_ir.name) {
            Some(x) => {
                let mut ty_var_table = TypeVariableTable::new();
                let func_q = x.ty.to_ty(struct_list, &mut ty_var_table, &mut program_ir.ty_env);
                program_ir.explicit_func_list.push(ExplicitFunc {
                    func: func_ir,
                    scheme: Scheme::Forall { qual: Qual { ps: func_q.ps, t: Type::create_func_type2(func_q.t) }, tgen_count: 0 },
                })
            }
            None => { program_ir.implicit_func_list.insert(func_ir.name.clone(), ImplicitFunc { func: func_ir }); }
        };
        var_table.out_nest();
        Ok(program_ir)
    }
}

impl DecFuncAST {
    fn to_mir(self, mut program_ir: ProgramMir, struct_list: &HashMap<String, DecStructHir>) -> ProgramMir {
        let mut ty_var_table = TypeVariableTable::new();
        let dec_func_ir = DecFuncMir {
            name: self.name,
            extern_flag: self.extern_flag,
            pos: self.pos,
            ty: self.ty.to_ty(struct_list, &mut ty_var_table, &mut program_ir.ty_env),
        };
        match self.extern_flag {
            true => {
                program_ir.ex_dec_func_list.push(dec_func_ir);
            }
            _ => panic!("error!"),
        }
        program_ir
    }
}

impl FuncTypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_env: &mut TypeEnv) -> Qual<FuncType> {
        let ret_q = self.ret_ty.to_ty(struct_list, ty_var_table, ty_env);
        let param_qs: Vec<_> = self.params_ty.into_iter()
            .map(|x| x.to_ty(struct_list, ty_var_table, ty_env)).collect();
        let (pss, param_ts) = Qual::split(param_qs);
        let mut ps = pss.into_iter().fold(HashMap::new(), |mut acc, ps| {
            acc.extend(ps.into_iter());
            acc
        });
        ps.extend(ret_q.ps.into_iter());
        Qual {
            t: FuncType {
                ret_type: ret_q.t,
                param_types: param_ts,
            },
            ps: Preds(ps),
        }
    }
}

impl TupleTypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_env: &mut TypeEnv) -> TupleType {
        let element_qs: Vec<_> = self.elements_ty.into_iter()
            .map(|x| x.to_ty(struct_list, ty_var_table, ty_env)).collect();
        let (_pss, element_ts) = Qual::split(element_qs);
        TupleType {
            element_tys: element_ts
        }
    }
}

impl RecordTypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_env: &mut TypeEnv) -> RecordType {
        let (names, element_qs): (Vec<_>, Vec<_>) = self.elements_ty.into_iter()
            .map(|(name, type_ast)|
                (name, type_ast.to_ty(struct_list, ty_var_table, ty_env))
            ).unzip();
        let (_pss, element_ts) = Qual::split(element_qs);
        RecordType {
            element_tys: names.into_iter().zip(element_ts.into_iter()).collect()
        }
    }
}

impl StructTypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_env: &mut TypeEnv) -> StructType {
        let internal_ty =
            match self.ty {
                StructInternalTypeAST::TupleTypeAST(x) => StructInternalType::TupleType(x.to_ty(struct_list, ty_var_table, ty_env)),
                StructInternalTypeAST::RecordTypeAST(x) => StructInternalType::RecordType(x.to_ty(struct_list, ty_var_table, ty_env))
            };
        StructType { ty: internal_ty, name: self.name }
    }
}

impl TypeAST {
    fn to_ty(self, struct_list: &HashMap<String, DecStructHir>, ty_var_table: &mut TypeVariableTable, ty_env: &mut TypeEnv) -> Qual<Type> {
        match self {
            TypeAST::Type(x) => Qual::new(x),
            TypeAST::FuncTypeAST(x) => {
                let ty_id = ty_env.fresh_type_id();
                let func_q = x.to_ty(struct_list, ty_var_table, ty_env);
                let cond = Condition::Call(Box::new(func_q.t));
                let mut ps = func_q.ps;
                ps.insert(Type::TyVar(ty_id.clone()), Pred { ty: Type::TyVar(ty_id.clone()), cond });
                Qual { t: Type::TyVar(ty_id), ps }
            }
            TypeAST::TupleTypeAST(x) => Qual::new(Type::TupleType(
                Box::new(x.to_ty(struct_list, ty_var_table, ty_env))
            )),
            TypeAST::TypeVarName(ty_name) => Qual::new(ty_var_table.get_ty(ty_name, ty_env)),
            TypeAST::StructTypeAST(x) => Qual::new(Type::StructType(Box::new(x.to_ty(struct_list, ty_var_table, ty_env)))),
            TypeAST::IdTypeAST(id) => {
                if struct_list.contains_key(&id) {
                    Qual::new(Type::StructType(Box::new((&struct_list[&id]).ty.clone().to_ty(struct_list, ty_var_table, ty_env))))
                } else {
                    panic!("実装めんどいな")
                }
            }
        }
    }
}

impl ExprAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        match self {
            ExprAST::NumAST(x) => Ok(ExprMir::NumMir(x)),
            ExprAST::BoolAST(x) => Ok(ExprMir::BoolMir(x)),
            ExprAST::IfAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::OpAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::VariableAST(x) => {
                match var_table.get_variable_ir(x.clone(), &mut program_ir.ty_env) {
                    Some(x) => Ok(x),
                    _ => Err(Error::new(x.pos, "not found variable")),
                }
            }
            ExprAST::ParenAST(x) => x.expr.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::FuncCallAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::NamedParamsConstructorCallAST(_) => panic!("bug!!"),
            ExprAST::TupleAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::TupleStructAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::LambdaAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::IndexPropertyAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
            ExprAST::NamePropertyAST(x) => x.to_mir(program_ir, struct_list, var_table, lambda_count),
        }
    }
}

impl IfAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(ExprMir::create_if_mir(
            self.cond.to_mir(program_ir, struct_list, var_table, lambda_count)?,
            self.t_expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
            self.f_expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
            self.pos,
            program_ir.ty_env.fresh_type_id(),
        ))
    }
}

impl OpAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(ExprMir::create_op_mir(
            self.op,
            self.l_expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
            self.r_expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
        ))
    }
}

impl FuncCallAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        let func = self.func.to_mir(program_ir, struct_list, var_table, lambda_count);

        let param = self
            .param.to_mir(program_ir, struct_list, var_table, lambda_count)?;
        Ok(ExprMir::create_call_mir(
            func?,
            vec![param],
            program_ir.ty_env.fresh_type_id(),
        ))
    }
}

impl TupleAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(ExprMir::create_tuple_mir(
            self.elements
                .into_iter()
                .map(|x| x.to_mir(program_ir, struct_list, var_table, lambda_count))
                .collect::<AstToIrResult<Vec<ExprMir>>>()?,
            self.pos,
            program_ir.ty_env.fresh_type_id(),
        ))
    }
}

impl TupleStructAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        let mut ty_var_table = TypeVariableTable::new();
        Ok(ExprMir::create_tuple_struct_mir(
            self.tuple.elements
                .into_iter()
                .map(|x| x.to_mir(program_ir, struct_list, var_table, lambda_count))
                .collect::<AstToIrResult<Vec<ExprMir>>>()?,
            self.tuple.pos,
            self.ty.to_ty(struct_list, &mut ty_var_table, &mut program_ir.ty_env),
            program_ir.ty_env.fresh_type_id(),
        ))
    }
}

impl LambdaAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        let env = self
            .env
            .iter()
            .map(
                |x| match var_table.get_variable_ir(x.clone(), &mut program_ir.ty_env) {
                    Some(ExprMir::VariableMir(x)) => Ok(x),
                    _ => Err(Error::new(x.pos, "Lambda capture not Local Variable")),
                },
            ).collect::<AstToIrResult<Vec<VariableMir>>>()?;
        let params_len: usize = if self.params.len() == 0 {
            1
        } else {
            self.params.len()
        };
        let params_len = params_len + self.env.len();
        let env_id_iter = self.env.iter().map(|x| x.id.clone());
        let params_id_iter = self.params.into_iter().map(|x| x.id);
        var_table.in_nest(&mut env_id_iter.chain(params_id_iter));
        let body = self.body.to_mir(program_ir, struct_list, var_table, lambda_count)?;
        var_table.out_nest();
        *lambda_count += 1;
        let lambda_name = "#".to_string() + &(*lambda_count - 1).to_string();
        program_ir.implicit_func_list.insert(lambda_name.clone(), ImplicitFunc {
            func: FuncMir {
                params_len,
                body,
                pos: self.pos,
                name: lambda_name.clone(),
            }
        });
        Ok(ExprMir::LambdaMir(Box::new(LambdaMir {
            env,
            func_name: lambda_name,
            ty_id: program_ir.ty_env.fresh_type_id(),
            func_id: program_ir.ty_env.fresh_type_id(),
            pos: self.pos,
            params_len,
        })))
    }
}

impl IndexPropertyAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(
            ExprMir::create_index_property_mir(
                self.expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
                self.pos,
                program_ir.ty_env.fresh_type_id(),
                self.index,
            )
        )
    }
}

impl NamePropertyAST {
    fn to_mir(
        self,
        program_ir: &mut ProgramMir,
        struct_list: &HashMap<String, DecStructHir>,
        var_table: &mut VariableTable,
        lambda_count: &mut usize,
    ) -> AstToIrResult<ExprMir> {
        Ok(
            ExprMir::create_name_property_mir(
                self.expr.to_mir(program_ir, struct_list, var_table, lambda_count)?,
                self.pos,
                program_ir.ty_env.fresh_type_id(),
                self.property_name,
            )
        )
    }
}

#[test]
fn ast_to_ir_test() {
    use super::type_inference::type_substitute::TypeSubstitute;
    use super::type_inference::type_env::TypeEnv;
    use combine::stream::state::SourcePosition;
    use compile::types::types::TypeId;
    let ast = ProgramAST {
        stmt_list: vec![StmtAST::DefFuncAST(DefFuncAST {
            name: "main".to_string(),
            params: vec![
                VariableAST::new("a".to_string(), SourcePosition { column: 0, line: 0 }),
                VariableAST::new("b".to_string(), SourcePosition { column: 0, line: 0 }),
            ],
            body: ExprAST::create_variable_ast(
                "b".to_string(),
                SourcePosition { column: 0, line: 0 },
            ),
            pos: SourcePosition { column: 0, line: 0 },
        })],
    };
    use indexmap::IndexMap;
    let mut func_list = IndexMap::new();
    func_list.insert("main".to_string(), ImplicitFunc {
        func: FuncMir {
            name: "main".to_string(),
            body: ExprMir::create_variable_mir(0, SourcePosition { line: 0, column: 0 }, TypeId::new(0)),
            params_len: 2,
            pos: SourcePosition { column: 0, line: 0 },
        }
    });
    let ir = ProgramMir {
        explicit_func_list: vec![],
        implicit_func_list: func_list,
        ex_dec_func_list: vec![],
        ty_sub: TypeSubstitute::new(),
        ty_env: TypeEnv::new(),
    };
    let ir2 = ast.to_hir().unwrap().to_mir().unwrap();
    assert_eq!(ir2.implicit_func_list, ir.implicit_func_list);
    assert_eq!(ir2.ex_dec_func_list, ir.ex_dec_func_list);
}
