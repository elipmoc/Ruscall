infixl 1 +;

main= merge succ succ 5;

merge f g x = f(g x);

succ x= x+1;