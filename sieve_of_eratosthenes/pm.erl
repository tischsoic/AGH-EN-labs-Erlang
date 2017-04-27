-module (pm).
-export ( [ pm/2, a/0, b/0, c/0, d/0 ]).
pm(list1, list2) -> 1;
pm(n, m) -> 2;
pm(X, X) -> 3;
pm( [ elem|elems ] , [ x|xs ]) -> 4;
pm( [ Elem|Elems ] , [ Elem|Elems ]) -> 5;
pm( [ _A|_As ] , [ _B|_Bs ]) -> 6;
pm( { _X } , _Y) -> 7;
pm( { A,B } , { B,A }) -> 8;
pm( { _A,_B } , { _C,_D }) -> 9;
pm(_X, _Y) -> 10.

a() -> pm( { a_atom } , another).
b() -> pm( [ elem,mints ] , [ candy,crush ]).
c() -> pm( { varied,flowers } , { flowers,varied }).
d() -> pm( [ cross, bow, service ] , [ cross, bow, service ]).
