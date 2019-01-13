#![recursion_limit = "128"]
#[macro_use]
extern crate combine;
extern  crate indexmap;
pub mod cmd_args;
pub mod compile;
pub mod hello;

fn main() {
    cmd_args::parse_cmd_args(cmd_args::get_cmd_args()).run();
}
