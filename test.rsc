/*
    自作言語Ruscall　
    サンプルコードだよ

    /*コメントネストもできるよ*/

*/

//　演算子優先順位定義
infixl 1 -;
infixl 1 +;
infixl 2 *;
infixl 0 ==;

ex print::Int32->Int32;

//　エントリーポイント
main = print(add 10 8);
add::Int32->Int32->Int32;
add x y= x+y;

func1=true;
func2=false;