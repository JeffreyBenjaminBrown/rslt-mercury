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

:- func fiveNumberSpace = list(int).
fiveNumberSpace = [1,2,3,4,5].

:- func testQElt = bool.
testQElt = Res :-
  solutions( runPQuery( fiveNumberSpace, qElt( 3 ) )
           , Solved )
  , Expected = set.from_list( [3] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

:- func testQFind = bool.
testQFind = Res :-
  solutions( runPQuery( fiveNumberSpace, qFind( list.filter( <(3) ) ) )
           , Solved )
  , Expected = set.from_list( [4,5] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

% todo : make into a lambda, or disappear via currying
:- func aCond( int ) = bool.
aCond( Elt ) = (if Elt > 3 then yes else no).

% almost works, except for the determinism.
%:- func testQCond = bool.
%testQCond = Res :-
%  QC = qCond( aCond )
%  , checkNQuery( fiveNumberSpace, QC, 5, Res ).

main(!IO) :-
   io.write_string( "testQElt: "  ++ string(testQElt)  ++ "\n", !IO),
   io.write_string( "testQFind: " ++ string(testQFind) ++ "\n", !IO).
