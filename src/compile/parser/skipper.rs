use combine::char::{newline, space, tab, string};
use combine::{many, many1,any,value};
use combine::parser::item::none_of;
use combine::parser::combinator::{try, not_followed_by};
use super::parser::MyStream;

//<skip>
parser! {
    pub fn skip_parser['a]()(MyStream<'a>)->()
    {
        newline()
        .or(space()
        .or(tab()))
        .map(|_|())
        .or(try(comment_parser()))
        .or(try(comments_parser()))
    }
}

//<skip_many>
parser! {
    pub fn skip_many_parser['a]()(MyStream<'a>)->()

    {
        many::<Vec<_>,_>(skip_parser()).map(|_|())
    }
}
//<skip_many1>
parser! {
    pub fn skip_many1_parser['a]()(MyStream<'a>)->()

    {
        many1::<Vec<_>,_>(skip_parser()).map(|_|())
    }
}

//<comment>
parser! {
    pub fn comment_parser['a]()(MyStream<'a>)->()
    {
        (
            string("//"),
            many::<Vec<_>,_>( none_of(vec!['\n'].into_iter()))
        ).map(|(_,_)|())
    }
}

//<comments>
parser! {
    pub fn comments_parser['a]()(MyStream<'a>)->()
    {
        (
            string("/*"),
            many::<Vec<_>,_>(
                try(comments_parser())
                .or(
                    not_followed_by(string("*/"))
                    .with(any())
                    .with(value(()))
                )
            ),
            string("*/")
        ).map(|(_,_,_)|())
    }
}