infixl 1 +;

//　エントリーポイント
//　add 4 10と等価
main = (swap( create())).1 ((swap( create())). 0.0) (10);

//タプル適当に作成
create = (add,(4 ,5) );

//足し算関数
add x y= x+y;

//タプル要素入れ替え
swap x= (x.1,x.0);
