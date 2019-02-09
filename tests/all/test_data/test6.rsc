infixl 3 +;

func::(Int32,Int32->Int32)->(Int32,(Int32,Int32->Int32));
func x= (4,x);
main = (func(4+3,hoge),0).0.0;

hoge x =x+7;