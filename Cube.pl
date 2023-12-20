isPrime(2) :-
    !.
isPrime(3) :-
    !.
isPrime(X) :-
    X > 3,
    X mod 2 =\= 0,
    isPrime_(X, 3).

isPrime_(X, N) :-
    ( N*N > X
    -> true
    ;  X mod N =\= 0,
       M is N + 2,
       isPrime_(X, M)
    ).
 



generator4([N]) :-
    permutation([0,1,2,3,4,5,6,7,8,9], removeBrackets(N)),
    checkPrimes(N)
    .

    
digits(N, [N]) :-
    N < 10.
digits(N, W) :-
    N >= 10,
    div_mod(N, 10, D, M),
    digits(D, R),
    append(R, [M], W).


x_generator4(N) :-
    x_generator4_loop(
        [ [[9 ,6 ,7] ,[4 ,0 ,1] ,[2 ,8 ,3] ,[5]]
        , [[9 ,8 ,3] ,[6 ,0 ,1] ,[5] ,[4 ,7] ,[2]]
        , [[9 ,8 ,3] ,[6 ,7] ,[4 ,2 ,0 ,1] ,[5]]
        , [[9 ,8 ,5 ,1] ,[2] ,[4 ,3] ,[6 ,0 ,7]]
        , [[9 ,8 ,5 ,1] ,[2] ,[3] ,[6 ,0 ,4 ,7]]
        , [[9 ,8 ,5 ,1] ,[2] ,[7] ,[4 ,6 ,0 ,3]]
        , [[8 ,9] ,[7] ,[6 ,0 ,1] ,[2 ,5 ,4 ,3]]
        , [[8 ,9] ,[7] ,[5 ,6 ,3] ,[4 ,0 ,2 ,1]]
        , [[8 ,9] ,[5] ,[4 ,7] ,[6 ,0 ,1] ,[3] ,[2]]
        , [[3] ,[5] ,[6 ,0 ,7] ,[2] ,[4 ,1] ,[8 ,9]] ] , 0 , N ) .

x_generator4_loop([], C, C).
x_generator4_loop([T|TS], C, N) :-
    generator4(T),
    C1 is C + 1 ,
x_generator4_loop(TS, C1, N).
x_generator4_loop([_|TS], C, N) :-
    x_generator4_loop(TS,C,N).



tester4(N) :-

number(L, N) :-
    number(L, 0, N).

number([], A, A).
number([H|T], A, N) :-
    C is 10*A + H,
    number(T, C, N).

insertPrimes(E, [], [E]).
insertPrimes(E, [H|T], [E,H|T]) :-
    number(E,NE),
    number(H,NH),
    NE > NH.
insertPrimes(E, [H|T], [H|W]) :-
    number(E,NE),
    number(H,NH),
    NE =< NH,
    insertPrimes(E,T,W).

sortPrimes([],[]).
sortPrimes([H|T], X) :-
    sortPrimes(T,W)
    insertPrimes(H,W,X).

x_tester4(N) :-
    x_tester4_loop(
        [[[8,2,7], [6,1], [5,3], [4,0,9]]
        , [[8,2,7], [6,1], [4,0,9], [5,3]]
        , [[8,2,7], [5,3], [6,1], [4,0,9]]
        , [[8,2,7], [4,0,9], [6,1], [5,3]]
        , [[6,1], [8,2,7], [4,0,9], [5,3]]
        , [[6,1], [4,0,9], [5,3], [8,2,7]]
        , [[5,3], [6,1], [4,0,9], [8,2,7]]
        , [[5,3], [4,0,9], [6,1], [8,2,7]]
        , [[4,0,9], [5,3], [8,2,7], [6,1]]
        , [[4,0,9], [8,2,7], [6,1], [5,3]] ], 0, N ).

x_tester4_loop([], C, C).
x_tester4_loop([T|TS], C, N) :-
    tester4(T),
    C1 is C + 1,
    x_tester4_loop(TS, C1, N).
x_tester4_loop([_|TS], C, N) :-
    x_tester4_loop(TS, C, N).