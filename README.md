# Ruscall
[![Build Status](https://travis-ci.org/elipmoc/Ruscall.svg?branch=develop)](https://travis-ci.org/elipmoc/Ruscall)
![license](https://img.shields.io/github/license/mashape/apistatus.svg)
[![Coverage Status](https://coveralls.io/repos/github/elipmoc/Ruscall/badge.svg)](https://coveralls.io/github/elipmoc/Ruscall)


自作言語処理系のコンパイラ

![ラスカル](https://raw.githubusercontent.com/elipmoc/Ruscall/develop/image.jpg "ラスカル")

# インストールガイド

## [Linux]

```
apt-get install llvm-6.0
apt-get install g++
LLVM_SYS_60_PREFIX=/usr/lib/llvm-6.0 cargo run
```

## [Windows]
めんどくさい。書く気になれん。

## 使い方

test.rscというサンプルソースファイルがあるのでそれを使ってみる。

```
cargo run -- -build test.rsc
```

実行するとcompiled.outが生成されます。

ファイル名をまだ指定できないんだ。‪本当に‬申し訳ない😢‬
# 目的
Rust、Scala、Haskellのいいところを結集した言語を作る。


## DFD
![dfd](https://raw.githubusercontent.com/elipmoc/Ruscall/develop/DFD.png "dfd")
