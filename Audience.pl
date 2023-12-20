perfect_square(N) :-
    between(1, inf, N),
    S is floor(sqrt(N)),
    N is S * S.

generator3(T) :-
    between(1000,1000000,T), 
    perfect_square(T).

x_generator3(N) :-
    x_generator3_loop(
        [1024, 9409, 23716, 51529
        , 123904, 185761, 868624, 962361
        , 982081, 1000000] , 0 , N ) .

x_generator3_loop([], C, C).
x_generator3_loop([T|TS], C, N) :-
    generator3(T),
    C1 is C + 1,
    x_generator3_loop(TS, C1, N).
x_generator3_loop([_|TS ], C, N) :-
    x_generator3_loop(TS, C, N).

tester3(N) :- 
    digits(N,NS),
    different(NS),
    member(0,NS),
    last_digit(NS),
    multiples_odd(NS).

different([]).
different([H|T]) :-
    \+ member(H, T), different(T).

digits(N, [N]) :-
    N < 10.
digits(N, W) :-
    N >= 10,
    div_mod(N, 10, D, M),
    digits(D, R),
    append(R, [M], W).
    
div_mod(A, B, D, M) :-
    D is A div B,
    M is A mod B.

last_digit(NS) :-
    length(NS,S),
    last(NS,L),
    L =:= S.

secondLast([X,_], X).
secondLast([_|T], X) :- 
    secondLast(T, X).

nThElem([_|LS], I, N) :-
    I > 1,
    nThElem(LS, I-1, N).
nThElem([L|_], _, L).

multiples_odd(NS) :-
    nThElem(NS,1,F),
    nThElem(NS,2,S),
    nThElem(NS,3,T),
    length(NS,L),
    nThElem(NS,L-1,SL),
    !,
    odd(SL),
    multiple(F,S),
    multiple(F,T),
    multiple(F,SL).

odd(N) :-
    1 is N mod 2.

multiple(F,M) :-
    M mod F =:= 0.

x_tester3(N) :-
x_tester3_loop(
    [123056, 128036, 139076, 142076
    , 148056, 159076, 173096, 189036
    , 193056, 198076], 0, N ).

x_tester3_loop([], C, C ).
x_tester3_loop([T|TS], C , N ) :-
    tester3(T),
    C1 is C + 1,
    x_tester3_loop(TS, C1, N).
x_tester3_loop([_|TS], C, N) :-
    x_tester3_loop(TS, C, N).

main :-
    generator3(N), tester3(N), write(N).