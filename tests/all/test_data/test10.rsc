infixl 1 +;

ex print::Int32->Int32;

main = print( add_five() 10 );

add::Int32->Int32->Int32;
add x y= x+y;

add_five::()->Int32->Int32;
add_five = add 5;
