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
}
