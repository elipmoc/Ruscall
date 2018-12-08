use super::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramHir {
    pub infix_list: HashMap<String, InfixAST>,
    pub def_func_list: HashMap<String, DefFuncHir>,
    pub dec_func_list: HashMap<String, DecFuncHir>,
    pub ex_dec_func_list: HashMap<String, ExDecFuncHir>,
    pub struct_list: HashMap<String, DecStructHir>,
}

pub type InfixHir = InfixAST;
pub type DefFuncHir = DefFuncAST;
pub type DecFuncHir = DecFuncAST;
pub type ExDecFuncHir = DecFuncAST;
pub type DecStructHir = DecStructAST;