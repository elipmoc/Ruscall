/*
struct Point{ x:Int32 , y:Point2 };
struct Point2{ x:Int32 , y:Int32 };

//　エントリーポイント
main = (Point 4 (Point2 3 2)).1.0;
*/

infixl 1 +;

ex print::Int32->Int32;

main = print( (func() add) 4 8);

func::()->(Int32->Int32->Int32)->Int32->Int32->Int32;
func =\f,a,b->f a b;

add x y=x+y;
