infixl 1 +;
infixl 2 *;
ex print::Int32->Int32;

main = print(a true 5 + a false 5);

a flag=
    if flag{
        (\x->x+10)
    } else {
        (\x->x*10)
    };