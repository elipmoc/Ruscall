infixl 1 +;

main = print(add 5 4  );

add::Int32->Fn Int32->Int32->(Fn Int32->Int32);
add x y=x+y;

