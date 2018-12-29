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
    Compile(String, String),
}

impl CmdArgsKind {
    //コマンドラインオプションによる実行
    pub fn run(self) {
        match self {
            CmdArgsKind::Help => {
                println!("\nUsage:");
                println!("Ruscall.exe [OPTIONS]");
                println!("\nOPTIONS:");
                println!("-h                    help");
                println!("-v                    version");
                println!("-hello                build hello world ");
                println!("-build [SOURCE_FILE]  build source file");
                println!("-build [SOURCE_FILE] [OUTPUT_FILE]  build source file and set output file name");
            }
            CmdArgsKind::Version => println!("\nRuscall version 0.6.0\n"),
            CmdArgsKind::Hello => hello::hello(),
            CmdArgsKind::Compile(ref input_file_name, ref output_file_name) => {
                if let Err(err) = compile::compile(input_file_name, output_file_name) {
                    eprintln!("{}", err);
                }
            }
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
            ("-build", file_name) => CmdArgsKind::Compile(file_name.to_string(), file_name.to_string()),
            _ => CmdArgsKind::Error,
        },
        4 => match (&*(args[1]), &*(args[2]), &*(args[3])) {
            ("-build", input_file_name, output_file_name) => CmdArgsKind::Compile(input_file_name.to_string(), output_file_name.to_string()),
            _ => CmdArgsKind::Error,
        }
        _ => CmdArgsKind::Error,
    }
}
