mod parser;
mod types;
mod skipper;

use super::ir::ast;
use combine::easy;
use combine::Parser;
use combine::stream::state::{SourcePosition, State};


pub fn parse(
    s: &str,
) -> Result<
    (
        ast::ProgramAST,
        State<&str, SourcePosition>,
    ),
    easy::Errors<char, &str, SourcePosition>,
> {
    parser::program_parser().easy_parse(State::new(s))
}