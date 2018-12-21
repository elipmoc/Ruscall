infixl 1 +;
ex print::Int32->Int32;

main = (add()) 5;
add = \x -> print(x)+1 ;
