infixl 1 +;
ex print::Int32->Int32;

main = print((merge (\x->x+10) (\x->x+5))1+((abc 100)1000)10000);

add x = x+5;
add2 x = x+10;

merge g f= \[f,g]x->g(f x);

abc a= \[a]b->\[a,b]c->a+b+c;