use combine::char::{newline, space, tab};
use combine::many;
use super::parser::MyStream;

//<skip>
parser! {
    pub fn skip_parser['a]()(MyStream<'a>)->()
    {
        newline().map(|_|()).or(space_parser())
    }
}

//<skip_many>
parser! {
    pub fn skip_many_parser['a]()(MyStream<'a>)->()

    {
        many::<Vec<_>,_>(skip_parser()).map(|_|())
    }
}

//<space>
parser! {
   pub fn space_parser['a]()(MyStream<'a>) ->()
    {
        space().or(tab()).map(|_|())
    }
}