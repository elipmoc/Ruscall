infixl 1 +;
infixl 2 *;
ex print::Int32->Int32;

main = print(merge (\x->x*5) (\x->x+10) 100);

merge g f= \[f,g]x->g(f x);