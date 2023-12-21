isPrime(A):-
    not((A1 is A-1,
        between(2,A1,N), 
        0 is mod(A,N))),
        not(A is 1).
 
checkPrime(P) :-
    length(P,L),
    L =< 4,
    number(P,N),
    isPrime(N).

number(L, N) :-
    number(L, 0, N).

number([], A, A).
number([H|T], A, N) :-
    C is 10*A + H,
    number(T, C, N).

primes(N) :-
    maplist(checkPrime, N).
    
removeBrackets([N],N).
removeBrackets([H|T],W) :-
    removeBrackets(T,U),
    append(H,U,W).

generator4(N) :- % only checks if correct format
    removeBrackets(N,L), 
    permutation([0,1,2,3,4,5,6,7,8,9], L),
    primes(N).

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

perfect_cube(N) :-
    between(1, inf, N),
    S is floor(N**(1/3)),
    N is S * S * S.

% cubes predicate checks for cubes in a list of numbers using perfect_cube

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
    sortPrimes(T,W),
    insertPrimes(H,W,X).


removeSmallest([_|NS],W) :-
    W is NS.


tester4(N) :-
    sortPrimes(N,NS),
    removeSmallest(NS,XS),
    removeBrackets(XS,_).
    %cubes(X)

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
