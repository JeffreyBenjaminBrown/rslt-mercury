:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- type pQuery ---> qElt( int )
      ; qFind( func( list(int) ) = list(int) ).

:- type nQuery ---> qNot( int )
      ; qCond( func( list(int), int ) = bool ).

:- pred runPQuery( list(int), pQuery, int ).
:- mode runPQuery( in,        in,     out ) is nondet.

:- pred testNQuery( list(int), nQuery, int, bool ).
:- mode testNQuery( in,        in,     in,  out ) is nondet.


:- implementation.

testNQuery( Space, qCond(Q) , Elt, Res ) :-
  Q( Space, Elt ) = Res.
testNQuery( Space, qNot(Int), Elt, yes ) :-
  list.member( Elt, Space )
  , Elt \= Int.
testNQuery( Space, qNot(Int), Elt, no  ) :-
  not( list.member( Elt, Space ) )
  ; Elt = Int.

runPQuery( List, qElt( Elt ), Elt ) :-
  list.member( Elt, List ).
runPQuery( List, qFind( Gen ), Elt ) :-
  Gen( List ) = Elts,
  list.member( Elt, Elts ).
