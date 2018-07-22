use std::env;

pub enum CmdArgsKind {
    Help,
    Version,
    Error,
}

impl CmdArgsKind {
    pub fn run(&self) {
        match self {
            CmdArgsKind::Help => {
                println!("");
                println!("Usage:");
                println!("Ruscall.exe [OPTIONS]");
                println!("");
                println!("OPTIONS:");
                println!("-h           help");
                println!("-v           version\n");
            }
            CmdArgsKind::Version => println!("\nRuscall version 0.1.0\n"),
            CmdArgsKind::Error => println!("\nerror cmd args\n"),
        }
    }
}

pub fn parse_cmd_args() -> CmdArgsKind {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        CmdArgsKind::Help
    } else if args.len() == 2 {
        match &*(args[1]) {
            "-h" => CmdArgsKind::Help,
            "-v" => CmdArgsKind::Version,
            _ => CmdArgsKind::Error,
        }
    } else {
        CmdArgsKind::Error
    }
}
