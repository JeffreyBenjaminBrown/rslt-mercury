:- module int_graph_forward_backward.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.


:- implementation.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

:- pred parent( int, int ).
:- mode parent( in, in ) is semidet.
:- mode parent( in, out ) is nondet.
:- mode parent( out, in ) is nondet.
parent( 1, 2 ).
parent( 2, 3 ).
parent( 3, 4 ).
parent( 4, 5 ).
parent( 3, 3 ).

:- pred ancestor( int, int ).
:- mode ancestor( in,  in  ) is semidet.
:- mode ancestor( in,  out ) is nondet.
:- mode ancestor( out, in  ) is nondet.
ancestor( X, Y ) :- parent( X, Y ).
ancestor( X, Y ) :- parent( X, Middle )
                  , parent( Middle, Y ).

main(!IO) :-
    solutions( pred(I::out) is nondet :- ancestor(3,I), Descendents )
  , solutions( pred(I::out) is nondet :- ancestor(I,3), Ancestors )
  , io.write_string(    string(Descendents) ++ "\n"
                     ++ string(Ancestors  ) ++ "\n"
                   , !IO ).
