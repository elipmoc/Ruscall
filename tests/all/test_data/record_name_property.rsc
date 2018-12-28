struct Point{
    x:(),
    y:Int32
};

struct Add{
    func:Int32->Int32->Int32 ,
};

infixl 1 +;

//　エントリーポイント
main =
    (create_add()).func
        (Point () 4).y
        7
    ;

create_add = Add{ func = \x,y->x+y };

fuga = hoge (4,());

hoge x = (x.0,x.0);
