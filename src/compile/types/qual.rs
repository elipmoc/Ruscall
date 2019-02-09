use super::*;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
//制約と型をセットにしたもの
//psが制約
//tがその制約がかけられたなにか
//例: ps: aはNum制約がある。 t:a->a
//説明：　Num制約がかかった型変数aがあり、型はa->aである
pub struct Qual<T> {
    pub ps: Preds,
    pub t: T,
}

impl<T> Qual<T> {
    pub fn new(t: T) -> Qual<T> {
        Qual { ps: Preds(HashMap::new()), t }
    }
    //複数のqualをpredとtでそれぞれ分割する。
    pub fn split(qs: Vec<Qual<T>>) -> (Vec<Preds>, Vec<T>) {
        let mut pss = Vec::with_capacity(qs.len());
        let mut ts = Vec::with_capacity(qs.len());
        for q in qs {
            pss.push(q.ps);
            ts.push(q.t);
        }
        (pss, ts)
    }
}