%-*-Prolog-*-


assertTrue(true,_Msg) :- !.
assertTrue(false, Msg) :-
    halt(Msg).


assertNot(false,Msg) :-!.
assertNot(true, Msg) :-
    halt(Msg).


    
assertEqual(A,B, Msg) :-
    A = B, !.
assertEqual(A,B, Msg) :-
    halt(Msg).

assertNotEqual(A,B, Msg) :-
    A \= B, !.
assertNotEqual(A,B, Msg) :-
    halt(Msg).




    


    