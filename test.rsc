infixl 1 +;
ex print::Int32->Int32;

main = print(((add 1)2)3);

add a= \[a]b->\[a,b]c->a+b+c;
