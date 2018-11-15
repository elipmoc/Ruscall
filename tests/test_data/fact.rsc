/*
    自作言語Ruscall　
    サンプルコードだよ

    /*コメントネストもできるよ*/

*/

//　演算子優先順位定義
infixl 1 -;
infixl 2 *;
infixl 0 ==;

//　extern宣言
ex print::Int32->Int32;

//　エントリーポイント
main = print(fact 5);

//　階乗の関数
fact x=
    if x==2-1{
        1
    } else {
        x * fact(x-1)
    };
