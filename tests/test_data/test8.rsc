infixl 1 +;
ex print::Int32->Int32;

main = (merge succ caller) succ;
merge f g=  \[f,g]x-> f(g x);

caller x = x 5;

succ x = x+1;