extern crate ruscall;

//helper
fn to_str_vec(args: Vec<&str>) -> Vec<String> {
    args.into_iter().map(|x| x.to_string()).collect()
}

#[test]
fn parse_cmd_args_test() {
    use ruscall::cmd_args::*;
    assert_eq!(
        parse_cmd_args(to_str_vec(vec!["my.exe"])),
        CmdArgsKind::Help
    );
    assert_eq!(
        parse_cmd_args(to_str_vec(vec!["my.exe", "-h"])),
        CmdArgsKind::Help
    );
    assert_eq!(
        parse_cmd_args(to_str_vec(vec!["my.exe", "-v"])),
        CmdArgsKind::Version
    );
    assert_eq!(
        parse_cmd_args(to_str_vec(vec!["my.exe", "-hoge"])),
        CmdArgsKind::Error
    );
    assert_eq!(
        parse_cmd_args(to_str_vec(vec!["my.exe", "-hello"])),
        CmdArgsKind::Hello
    );
    assert_eq!(
        parse_cmd_args(to_str_vec(vec!["my.exe", "-build", "hoge"])),
        CmdArgsKind::Compile("hoge".to_string())
    );
}

use ruscall::compile::ast;
//helper
fn create_infixl_ast(op: &str, priority: i8) -> ast::StmtAST {
    ast::StmtAST::InfixAST(ast::InfixAST {
        op: op.to_string(),
        ty: ast::InfixType::Left,
        priority: ast::Priority(priority),
    })
}
fn create_infixr_ast(op: &str, priority: i8) -> ast::StmtAST {
    ast::StmtAST::InfixAST(ast::InfixAST {
        op: op.to_string(),
        ty: ast::InfixType::Right,
        priority: ast::Priority(priority),
    })
}

#[test]
fn infix_parse_test() {
    use ruscall::compile::parser::parse;
    assert_eq!(
        parse("infixl 4 +;").unwrap().0,
        ast::ProgramAST {
            stmt_list: vec![create_infixl_ast("+", 4)],
        }
    );
    assert_eq!(
        parse("infixr 7 /;").unwrap().0,
        ast::ProgramAST {
            stmt_list: vec![create_infixr_ast("/", 7)],
        }
    );
    assert_eq!(
        parse("  infixr 7 / \n; infixl 9 * ;\tinfixr 1 -;").unwrap().0,
        ast::ProgramAST {
            stmt_list: vec![
                create_infixr_ast("/", 7),
                create_infixl_ast("*", 9),
                create_infixr_ast("-", 1),
            ],
        }
    );
}
