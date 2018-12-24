use super::super::ir::ast::*;
use super::super::ir::hir::*;
use super::super::Error;
use std::collections::HashMap;

type ResolveResult<T> = Result<T, Error>;

//struct RGB{ B:Int32 , G:Int32 , R:Int32 }
//RGB {R:5,G:3,B:2}のようなレコード構造体生成関数呼び出しを
//RGB 2 3 5のように脱糖衣する


impl ProgramHir {
    pub fn resolve_named_params_constructor_call(mut self) -> ResolveResult<ProgramHir> {
        let struct_list = self.struct_list;
        self.def_func_list =
            self.def_func_list.into_iter()
                .map(|(k, f)| Ok((k, f.resolve_named_params_constructor_call(&struct_list)?)))
                .collect::<ResolveResult<HashMap<String, DefFuncHir>>>()?;
        self.struct_list = struct_list;
        Ok(self)
    }
}

impl DefFuncHir {
    fn resolve_named_params_constructor_call(mut self, struct_list: &HashMap<String, DecStructHir>) -> ResolveResult<DefFuncHir> {
        self.body = self.body.resolve_named_params_constructor_call(struct_list)?;
        Ok(self)
    }
}

impl ExprAST {
    fn resolve_named_params_constructor_call(mut self, struct_list: &HashMap<String, DecStructHir>) -> ResolveResult<ExprAST> {
        match self {
            ExprAST::NamedParamsConstructorCallAST(x) => {
                let mut x = *x;
                if struct_list.contains_key(&x.constructor_name) == false { return Err(Error::new(x.pos, "not found constructor")); }
                let struct_ty = &struct_list[&x.constructor_name].ty;
                match struct_ty.ty {
                    StructInternalTypeAST::TupleTypeAST(_) => return Err(Error::new(x.pos, "not RecordStruct")),
                    StructInternalTypeAST::RecordTypeAST(ref y) => {
                        let result = x.params.into_iter()
                            .map(|(expr_name, expr)| {
                                let expr = expr.resolve_named_params_constructor_call(struct_list)?;
                                Ok(
                                    y.elements_ty.iter().enumerate()
                                        .find_map(|(idx, (name, _))| {
                                            if *name == expr_name {
                                                Some(idx)
                                            } else { None }
                                        })
                                        .map(|idx| (idx, expr))
                                )
                            })
                            .collect::<ResolveResult<Option<Vec<_>>>>()?
                            .map(|mut x| {
                                x.sort_by(|(idx1, _), (idx2, _)| idx1.cmp(idx2));
                                x
                            });
                        match result {
                            Some(sorted_params) => {
                                self = sorted_params.into_iter()
                                    .fold(ExprAST::create_variable_ast(x.constructor_name, x.pos), move |acc, (_, param)|
                                        ExprAST::create_func_call_ast(acc, param),
                                    );
                            }
                            None => return Err(Error::new(x.pos, "not found record name"))
                        }
                    }
                }
            }
            ExprAST::BoolAST(_) | ExprAST::NumAST(_) | ExprAST::VariableAST(_) => (),
            ExprAST::FuncCallAST(x) => {
                let mut x = *x;
                x.func = x.func.resolve_named_params_constructor_call(struct_list)?;
                x.param = x.param.resolve_named_params_constructor_call(struct_list)?;
                self = ExprAST::FuncCallAST(Box::new(x));
            }
            ExprAST::IfAST(x) => {
                let mut x = *x;
                x.cond = x.cond.resolve_named_params_constructor_call(struct_list)?;
                x.f_expr = x.f_expr.resolve_named_params_constructor_call(struct_list)?;
                x.t_expr = x.t_expr.resolve_named_params_constructor_call(struct_list)?;
                self = ExprAST::IfAST(Box::new(x));
            }
            ExprAST::LambdaAST(x) => {
                let mut x = *x;
                x.body = x.body.resolve_named_params_constructor_call(struct_list)?;
                self = ExprAST::LambdaAST(Box::new(x));
            }
            ExprAST::OpAST(x) => {
                let mut x = *x;
                x.l_expr = x.l_expr.resolve_named_params_constructor_call(struct_list)?;
                x.r_expr = x.r_expr.resolve_named_params_constructor_call(struct_list)?;
                self = ExprAST::OpAST(Box::new(x));
            }
            ExprAST::ParenAST(x) => {
                let mut x = *x;
                x.expr = x.expr.resolve_named_params_constructor_call(struct_list)?;
                self = ExprAST::ParenAST(Box::new(x));
            }
            ExprAST::TupleAST(x) => {
                let mut x = *x;
                x.elements = x.elements.into_iter()
                    .map(|x| { x.resolve_named_params_constructor_call(struct_list) })
                    .collect::<ResolveResult<Vec<_>>>()?;
                self = ExprAST::TupleAST(Box::new(x));
            }
            ExprAST::TupleStructAST(x) => {
                let mut x = *x;
                x.tuple.elements = x.tuple.elements.into_iter()
                    .map(|x| { x.resolve_named_params_constructor_call(struct_list) })
                    .collect::<ResolveResult<Vec<_>>>()?;
                self = ExprAST::TupleStructAST(Box::new(x));
            }
            ExprAST::TuplePropertyAST(x) => {
                let mut x = *x;
                x.expr = x.expr.resolve_named_params_constructor_call(struct_list)?;
                self = ExprAST::TuplePropertyAST(Box::new(x));
            }
        }
        Ok(self)
    }
}