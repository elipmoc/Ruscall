 infixl
    1
    +
    ;

ex print::Int32->Int32;
ex scan::Int32->Int32;

 main
    =
        print2
            print
            (
                add
                    (scan 0)
                    (scan 0)
            )
;

add x y =x+y;

print2 p x = p x;

