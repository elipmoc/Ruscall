
struct Point(Int32 , ());
struct Point2{ x:(), y:Int32 };
infixl 1 +;

//　エントリーポイント
main =
    hoge
        (Point 4 ())
        Point2{ y=16 , x=() };

hoge x y = x.0 + y.1;
