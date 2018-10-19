use super::compile;
use super::hello;
use std::env;

//コマンドラインオプションの種類
#[derive(Debug, PartialEq)]
pub enum CmdArgsKind {
    Help,
    Version,
    Error,
    Hello,
    Compile(String),
}

impl CmdArgsKind {
    //コマンドラインオプションによる実行
    pub fn run(self) {
        match self {
            CmdArgsKind::Help => {
                println!("");
                println!("Usage:");
                println!("Ruscall.exe [OPTIONS]");
                println!("");
                println!("OPTIONS:");
                println!("-h                    help");
                println!("-v                    version");
                println!("-hello                hello world build");
                println!("-build [SOURCE_FILE]  build source file");
            }
            CmdArgsKind::Version => println!("\nRuscall version 0.3.0\n"),
            CmdArgsKind::Hello => hello::hello(),
            CmdArgsKind::Compile(ref file_name) => compile::compile(file_name),
            CmdArgsKind::Error => println!("\nerror cmd args\n"),
        }
    }
}

//コマンドライン引数取得
pub fn get_cmd_args() -> Vec<String> {
    env::args().collect()
}

//コマンドライン引数からCmdArgsKindを生成
pub fn parse_cmd_args(args: Vec<String>) -> CmdArgsKind {
    match args.len() {
        1 => CmdArgsKind::Help,
        2 => match &*(args[1]) {
            "-h" => CmdArgsKind::Help,
            "-v" => CmdArgsKind::Version,
            "-hello" => CmdArgsKind::Hello,
            _ => CmdArgsKind::Error,
        },
        3 => match (&*(args[1]), &*(args[2])) {
            ("-build", file_name) => CmdArgsKind::Compile(file_name.to_string()),
            _ => CmdArgsKind::Error,
        },
        _ => CmdArgsKind::Error,
    }
}
