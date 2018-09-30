 infixl
    1
    +
    ;

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

