use super::super::ir::hir::*;
use super::super::ir::ast::*;

//Hirを組み替えてカリー化を行う

impl ProgramHir {
    pub fn create_constructor(mut self) -> ProgramHir {
        use combine::stream::state::SourcePosition;
        {
            let constructor_list =
                self.struct_list.iter()
                    .map(|(k, v)| {
                        let param_len = match v.ty.ty {
                            StructInternalTypeAST::RecordTypeAST(ref x) => x.elements_ty.len(),
                            StructInternalTypeAST::TupleTypeAST(ref x) => x.elements_ty.len()
                        };
                        let params =
                            (0..param_len)
                                .map(|id| VariableAST { id: id.to_string(), pos: SourcePosition::new() })
                                .collect::<Vec<_>>();
                        (
                            k.clone(),
                            DefFuncAST {
                                name: k.clone(),
                                pos: SourcePosition::new(),
                                params: params.clone(),
                                body: ExprAST::create_tuple_struct_ast(params.into_iter().map(|x| ExprAST::VariableAST(x)).collect(), v.ty.clone()),
                            },
                        )
                    });
            self.def_func_list.extend(constructor_list);
        }
        self
    }
}
