use combine::easy;
use combine::stream::state::SourcePosition;

#[derive(Debug)]
pub struct Error {
    pos: SourcePosition,
    msg: String,
}

type ParseError<'a> = easy::Errors<char, &'a str, SourcePosition>;

impl Error {
    pub fn from_parse_error(err: ParseError) -> Error {
        Error {
            pos: err.position,
            msg: err
                .errors
                .into_iter()
                .fold("".to_string(), { |acc, x| format!("{}\r\n{:?}", acc, x) }),
        }
    }
    pub fn to_string(self) -> String {
        format!(
            "\ncompile error!\n\nposition:\nline:{} column:{}\n\nmessage:\n{}\n",
            self.pos.line, self.pos.column, self.msg
        )
    }
    pub fn new(pos: SourcePosition, msg: &str) -> Error {
        Error {
            pos,
            msg: msg.to_string(),
        }
    }
}
