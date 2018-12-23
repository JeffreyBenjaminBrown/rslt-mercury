:- module test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.


:- implementation.
:- import_module int.
:- import_module list.
:- import_module query.
:- import_module solutions.

:- func aList = list(int).
aList = [1,2,3,4,5].

:- func aFilter( list(int) ) = list(int).
aFilter(List) = list.filter( <(3), List ).

main(!IO) :-
  solutions( runPQuery( aList, qElt( 3 ) ), S1 ),
  io.write_string( "S1: "   , !IO),
  io.print( S1, !IO ),
  solutions( runPQuery( aList, qFind( aFilter ) ), S2 ),
  io.write_string( "\nS2: "   , !IO),
  io.print( S2, !IO ),
  io.write_string( "\n", !IO ).
