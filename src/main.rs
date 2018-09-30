#[macro_use]
extern crate combine;
pub mod cmd_args;
pub mod compile;
pub mod hello;
pub mod my_llvm;

fn main() {
    cmd_args::parse_cmd_args(cmd_args::get_cmd_args()).run();
}
