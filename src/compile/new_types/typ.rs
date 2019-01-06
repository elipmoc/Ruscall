use std::fmt;
use super::types::*;
use super::traits::Qual;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Id(String);

impl Id {
    pub fn new(id: String) -> Id {
        Id(id)
    }
    pub fn with_str(id: &str) -> Id {
        Id(id.to_string())
    }
    pub fn enum_id(id: u32) -> Id {
        Id("v".to_string() + &(id.to_string()))
    }
    pub fn get_id(&self) -> &String {
        &self.0
    }
}

//カインド
#[derive(Clone, PartialEq, Hash, Eq)]
pub enum Kind {
    Star,
    Kfun(Box<Kind>, Box<Kind>),
}

impl fmt::Debug for Kind
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", match self {
            Star => "*".to_string(),
            Kfun(a, b) => format!("{:?} -> {:?}", a, b)
        })
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    //型変数
    TVar(TyVar),
    //型コンストラクタ
    TCon(TyCon),
    //型適応
    TAp(Box<Type>, Box<Type>),
    //量化した型変数
    TGen(u32),
}

impl Type {
    //量化変数なしの型スキームを生成
    pub fn to_scheme(self) -> Scheme {
        Forall { kinds: vec![], qual: Qual::new(vec![], self) }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
//型変数
pub struct TyVar {
    id: Id,
    kind: Kind,
}

impl TyVar {
    pub fn new(id: Id, kind: Kind) -> TyVar {
        TyVar { id, kind }
    }
}

#[derive(Clone, PartialEq, Debug)]
//型コンストラクタ
pub struct TyCon {
    id: Id,
    kind: Kind,
}

impl TyCon {
    pub fn new(id: Id, kind: Kind) -> TyCon {
        TyCon { id, kind }
    }
}

#[derive(Clone, PartialEq)]
//型スキーム
pub enum Scheme {
    Forall { kinds: Vec<Kind>, qual: Qual<Type> }
}

impl Scheme {
    //型変数を量化する
    fn quantify(vs: Vec<TyVar>, qt: Qual<Type>) -> Scheme {
        let vs =
            qt.tv()
                .into_iter()
                .filter(|v| vs.contains(&v))
                .collect::<Vec<_>>();
        let ks = vs.iter().map(|v| v.kind().clone()).collect::<Vec<_>>();
        let s = Subst::many_with(
            vs.into_iter().enumerate()
                .map(|(index, v)| (v.clone(), TGen(index as u32))).collect::<Vec<_>>());
        Forall { kinds: ks, qual: qt.apply(&s) }
    }
}

//変数の仮定
pub struct Assump {
    id: Id,
    scheme: Scheme,
}

//仮定のリスト
pub struct AssumpList(Vec<Assump>);

impl AssumpList {
    fn find_scheme(&self, id: &Id) -> Result<Scheme, String> {
        match self.0.iter().find(|a| &a.id == id) {
            Some(a) => Ok(a.scheme.clone()),
            None => Err("unbound identifier:".to_string() + id.get_id())
        }
    }
}

use self::Kind::*;
use self::Type::*;
use self::Scheme::*;

//helper---------------------------------------------------------------------

pub fn create_fn(a: Type, b: Type) -> Type {
    let (a, b) =
        (Box::new(a), Box::new(b));
    TAp(
        Box::new(TAp(Box::new(t_arrow()), a)),
        b,
    )
}

pub fn create_list(a: Type) -> Type {
    TAp(Box::new(t_list()), Box::new(a))
}

pub fn create_pair(a: Type, b: Type) -> Type {
    TAp(Box::new(TAp(Box::new(t_tuple2()), Box::new(a))), Box::new(b))
}

// * -> *
pub fn t_arrow() -> Type {
    TCon(TyCon::new(
        Id::with_str("(->)"),
        Kfun(
            Box::new(Star),
            Box::new(Kfun(Box::new(Star), Box::new(Star))),
        ),
    ))
}

// [*]
pub fn t_list() -> Type {
    TCon(TyCon::new(
        Id::with_str("[]"),
        Kfun(Box::new(Star), Box::new(Star)),
    ))
}

// (*,*)
pub fn t_tuple2() -> Type {
    TCon(TyCon::new(
        Id::with_str("(,)"),
        Kfun(Box::new(Star), Box::new(Kfun(Box::new(Star), Box::new(Star)))),
    ))
}

//*
pub fn t_int() -> Type {
    TCon(TyCon::new(Id::with_str("Int"), Star))
}

//トレイト------------------------------------
impl Types for Type {
    fn apply(&self, s: &Subst) -> Self {
        match self {
            TVar(v) =>
                match s.get(v) {
                    Some(t) => t.clone(),
                    None => self.clone()
                },
            TAp(a, b) => TAp(Box::new(a.apply(s)), Box::new(b.apply(s))),
            _ => self.clone()
        }
    }
    fn tv(&self) -> HashSet<&TyVar> {
        match self {
            TVar(v) => one_hash_set(v),
            TAp(a, b) => hash_set_union(a.tv(), &b.tv()),
            _ => HashSet::new()
        }
    }
}

impl Types for Scheme {
    fn apply(&self, s: &Subst) -> Self {
        match self {
            Forall { kinds, qual } => {
                Forall { kinds: kinds.clone(), qual: qual.apply(s) }
            }
        }
    }
    fn tv(&self) -> HashSet<&TyVar> {
        match self {
            Forall { ref qual, .. } => {
                qual.tv()
            }
        }
    }
}

impl Types for Assump {
    fn apply(&self, s: &Subst) -> Self {
        Assump { id: self.id.clone(), scheme: self.scheme.apply(s) }
    }
    fn tv(&self) -> HashSet<&TyVar> {
        self.scheme.tv()
    }
}

//型からカインドを得る
pub trait HasKind {
    fn kind(&self) -> &Kind;
}

impl HasKind for TyVar {
    fn kind(&self) -> &Kind { &self.kind }
}

impl HasKind for TyCon {
    fn kind(&self) -> &Kind { &self.kind }
}

impl HasKind for Type {
    fn kind(&self) -> &Kind {
        match self {
            TCon(tc) => tc.kind(),
            TVar(v) => v.kind(),
            TAp(t, _) =>
                match t.kind() {
                    Kfun(_, k) => k,
                    _ => panic!("error!")
                }
            _ => panic!("error!")
        }
    }
}


#[test]
fn example() {
    let t_unit = TCon(TyCon::new(Id::with_str("()"), Star));
    let t_char = TCon(TyCon::new(Id::with_str("Char"), Star));
    let t_int = TCon(TyCon::new(Id::with_str("Int"), Star));

    assert_eq!(TAp(
        Box::new(TAp(Box::new(t_arrow()), Box::new(t_int))),
        Box::new(TAp(Box::new(t_list()), Box::new(TVar(TyVar::new(Id::with_str("a"), Star))))),
    ).kind(), &Star);
}

