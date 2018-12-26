:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- type pQuery ---> qElt( int )
      ; qFind( func( list(int) ) = list(int) ).

:- type nQuery ---> qNot( int )
      ; qCond( func( int ) = bool).

:- pred runPQuery( list(int), pQuery, int ).
:- mode runPQuery( in,        in,     out ) is nondet.

:- pred checkNQuery( list(int), nQuery, int, bool ).
:- mode checkNQuery( in,        in,     in,  out ) is nondet.


:- implementation.

checkNQuery( Space, qCond(Q) , Elt, Res ) :-
  list.member( Elt, Space )
  , Q( Elt ) = Res.
checkNQuery( Space, qNot(Int), Elt, yes ) :-
  list.member( Elt, Space )
  , Elt \= Int.
checkNQuery( Space, qNot(Int), Elt, no  ) :-
  not( list.member( Elt, Space ) )
  ; Elt = Int.

runPQuery( List, qElt( Elt ), Elt ) :-
  list.member( Elt, List ).
runPQuery( List, qFind( Gen ), Elt ) :-
  Gen( List ) = Elts,
  list.member( Elt, Elts ).
