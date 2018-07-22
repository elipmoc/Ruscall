use super::hello;
use std::env;
//コマンドラインオプションの種類
#[derive(Debug, PartialEq)]
pub enum CmdArgsKind {
    Help,
    Version,
    Error,
    Hello,
}

impl CmdArgsKind {
    //コマンドラインオプションによる実行
    pub fn run(&self) {
        match self {
            &CmdArgsKind::Help => {
                println!("");
                println!("Usage:");
                println!("Ruscall.exe [OPTIONS]");
                println!("");
                println!("OPTIONS:");
                println!("-h           help");
                println!("-v           version\n");
            }
            &CmdArgsKind::Version => println!("\nRuscall version 0.1.0\n"),
            &CmdArgsKind::Hello => hello::hello(),
            &CmdArgsKind::Error => println!("\nerror cmd args\n"),
        }
    }
}

//コマンドライン引数取得
pub fn get_cmd_args() -> Vec<String> {
    env::args().collect()
}

//コマンドライン引数からCmdArgsKindを生成
pub fn parse_cmd_args(args: Vec<String>) -> CmdArgsKind {
    if args.len() == 1 {
        CmdArgsKind::Help
    } else if args.len() == 2 {
        match &*(args[1]) {
            "-h" => CmdArgsKind::Help,
            "-v" => CmdArgsKind::Version,
            "-hello" => CmdArgsKind::Hello,
            _ => CmdArgsKind::Error,
        }
    } else {
        CmdArgsKind::Error
    }
}
