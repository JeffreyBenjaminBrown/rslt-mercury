:- module query.
:- interface.
:- import_module int.
:- import_module list.

:- type pQuery ---> qElt( int )
      ; qFind( func( list(int) ) = list(int) ).

:- pred runPQuery( list(int), pQuery, int ).
:- mode runPQuery( in, in, out ) is nondet.


:- implementation.

runPQuery( List, qElt( Elt ), Elt ) :-
  list.member( Elt, List ).
runPQuery( List, qFind( Gen ), Elt ) :-
  Gen( List ) = Elts,
  list.member( Elt, Elts ).
