infixl 1 +;

ex print::Int32->Int32;

main = print( (func() add) 4 8);

func::()->(Int32->Int32->Int32)->Int32->Int32->Int32;
func =\f,a,b->f a b;

add x y=x+y;
