:- module toy.
:- interface.
:- import_module io.
:- import_module int.
:- import_module list.

:- pred main(io::di, io::uo) is det.

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

main(!IO) :-
  io.write_string( "testing\n"   , !IO).
