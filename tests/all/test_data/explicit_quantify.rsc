infixl 1 +;

ex print::Int32->Int32;

main = (id 4)+ ((id  (swap (3,2))  ).1);

id::a->a;
id x= x;

swap::(a,b)->(b,a);
swap x = (x.1,x.0);