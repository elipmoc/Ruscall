infixl 1 +;

ex print::Int32->Int32;

merge f g x= f(g x);

main =print ( (merge print (add 5) 10)+ (merge swap swap ((),10)).1);

add x y=x+y;

swap x = (x.1,x.0);