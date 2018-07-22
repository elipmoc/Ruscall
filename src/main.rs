mod cmd_args;
mod hello;
mod my_llvm;

fn main() {
    cmd_args::parse_cmd_args(cmd_args::get_cmd_args()).run();
}
