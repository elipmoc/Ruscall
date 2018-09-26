use std::collections::HashMap;
use super::super::types;
use super::ir;

impl ir::ProgramIr {
    pub fn ty_check(mut self) -> ir::ProgramIr {
        let func_name_list: Vec<String> =
            self.func_list.values().clone()
                .into_iter().map(|funcir| {
                funcir.name.clone()
            }).collect();
        for func_name in func_name_list {
            let func_ir = self.func_list.remove(&func_name).unwrap();
            func_ir.ty_check(&mut self.func_list, types::Type::Unknown);
        }
        self
    }
}

impl ir::FuncIr {
    fn ty_check(mut self, func_list: &mut HashMap<String, ir::FuncIr>, expect_ty: types::Type) -> types::Type {
        let mut expect_ret_ty = types::Type::Unknown;
        match expect_ty {
            types::Type::Unknown => (),
            types::Type::Fn(x) => match *x {
                types::Type::FuncType(x) => {
                    let x = *x;
                    expect_ret_ty = x.ret_type;
                    if x.param_types.len() != self.params.len() {
                        panic!("type error!");
                    }
                    self.params = self.params
                        .into_iter()
                        .zip(x.param_types)
                        .map(move |((id, ty), expect_ty)| {
                            match ty.merge(expect_ty) {
                                Option::Some(ty) => (id, ty),
                                _ => panic!("type error!")
                            }
                        }).collect();
                }
                _ => panic!("type error!")
            }
            _ => panic!("type error!")
        }
        self.body.ty_check(func_list, &mut self.params, expect_ret_ty);
        self.ty =
            types::Type::create_func_type(
                self.params.iter().map(|x| x.1.clone()).collect(),
                self.body.get_ty().clone(),
            );
        let ty = self.ty.clone();
        func_list.insert(self.name.clone(), self);
        ty
    }
}


impl ir::ExprIr {
    fn ty_check(
        &mut self,
        func_list: &mut HashMap<String, ir::FuncIr>,
        params: &mut Vec<(usize, types::Type)>,
        expect_ty: types::Type,
    ) -> types::Type {
        match self {
            ir::ExprIr::NumIr(x) => x.ty_check(),
            ir::ExprIr::CallIr(x) => x.ty_check(func_list, params, expect_ty),
            ir::ExprIr::OpIr(x) => x.ty_check(func_list, params),
            ir::ExprIr::VariableIr(x) => x.ty_check(params, expect_ty),
            ir::ExprIr::GlobalVariableIr(x) => x.ty_check(func_list, expect_ty),
        }
    }
}

impl ir::NumIr {
    fn ty_check(&mut self) -> types::Type {
        if self.ty == types::Type::Unknown {
            self.ty = types::Type::Int32;
        };
        self.ty.clone()
    }
}

impl ir::CallIr {
    fn ty_check(
        &mut self,
        func_list: &mut HashMap<String, ir::FuncIr>,
        params: &mut Vec<(usize, types::Type)>,
        expect_ty: types::Type,
    ) -> types::Type {
        for i in 0..self.params.len() {
            self.params[i].ty_check(func_list, params, types::Type::Unknown);
        }
        let expect_func_ty = types::Type::create_fn_func_type(
            self.params.iter().map(|x| x.get_ty().clone()).collect(),
            expect_ty.clone(),
        );

        self.func.ty_check(func_list, params, expect_func_ty);

        match self.func.get_ty() {
            types::Type::Fn(x) => match &**x {
                types::Type::FuncType(x) => self.ty = x.ret_type.clone(),
                _ => panic!("type error!")
            },
            _ => panic!("type error!")
        };

        self.ty.clone()
    }
}

impl ir::OpIr {
    fn ty_check(&mut self, func_list: &mut HashMap<String, ir::FuncIr>, params: &mut Vec<(usize, types::Type)>) -> types::Type {
        if self.ty != types::Type::Unknown {
            return self.ty.clone();
        }
        if self.l_expr.ty_check(func_list, params, types::Type::Int32) == types::Type::Int32
            && self.r_expr.ty_check(func_list, params, types::Type::Int32) == types::Type::Int32 {
            self.ty = types::Type::Int32;
        } else { panic!("type error!"); }
        self.ty.clone()
    }
}

impl ir::VariableIr {
    fn ty_check(&mut self, params: &mut Vec<(usize, types::Type)>, expect_ty: types::Type) -> types::Type {
        let mut param = params[params.len() - self.id - 1].clone();
        match param.1.merge(expect_ty) {
            Option::Some(ty) => param = (param.0, ty),
            _ => panic!("type error!")
        }
        let len = params.len();
        params[len - self.id - 1] = param.clone();
        self.ty = param.1;
        self.ty.clone()
    }
}

impl ir::GlobalVariableIr {
    fn ty_check(&mut self, func_list: &mut HashMap<String, ir::FuncIr>, expect_ty: types::Type) -> types::Type {
        let func_ir = func_list.remove(&self.id).unwrap();
        self.ty = types::Type::Fn(Box::new(func_ir.ty_check(func_list, expect_ty)));
        self.ty.clone()
    }
}