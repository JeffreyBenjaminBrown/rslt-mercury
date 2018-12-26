:- module test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.


:- implementation.
:- import_module int.
:- import_module list.
:- import_module query.
:- import_module solutions.

:- import_module bool.
:- import_module set.
:- import_module string.

:- func aSpace = list(int).
aSpace = [1,2,3,4,5].

:- func testQElt = bool.
testQElt = Res :-
  solutions( runPQuery( aSpace, qElt( 3 ) )
           , Solved )
  , Expected = set.from_list( [3] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

:- func aFilter( list(int) ) = list(int).
aFilter(List) = list.filter( <(3), List ).

:- func testQFind = bool.
testQFind = Res :-
  solutions( runPQuery( aSpace, qFind( aFilter ) )
           , Solved )
  , Expected = set.from_list( [4,5] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

main(!IO) :-
   io.write_string( "testQElt: "  ++ string(testQElt)  ++ "\n", !IO),
   io.write_string( "testQFind: " ++ string(testQFind) ++ "\n", !IO).
