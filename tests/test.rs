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

#[test]
fn infix_parse_test() {
    use ruscall::compile::ast;
    use ruscall::compile::parser::parse;
    assert_eq!(
        parse("infixl 4 +").unwrap().0,
        ast::ProgramAST {
            stmt_list: vec![ast::StmtAST::create_infixl_ast("+".to_string(), 4)],
        }
    );
    assert_eq!(
        parse("infixr 7 /").unwrap().0,
        ast::ProgramAST {
            stmt_list: vec![ast::StmtAST::create_infixr_ast("/".to_string(), 7)],
        }
    );
    assert_eq!(
        parse("  infixr 7 / \n infixl 9 * \tinfixr 1 -").unwrap().0,
        ast::ProgramAST {
            stmt_list: vec![
                ast::StmtAST::create_infixr_ast("/".to_string(), 7),
                ast::StmtAST::create_infixl_ast("*".to_string(), 9),
                ast::StmtAST::create_infixr_ast("-".to_string(), 1),
            ],
        }
    );
}
