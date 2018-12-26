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
  runPQuery( fiveNumberSpace
           , qElt( 3 )
           , Solved )
  , Expected = set.from_list( [3] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

:- func testQFind = bool.
testQFind = Res :-
  runPQuery( fiveNumberSpace
            , qFind( list.filter( <(3) ) )
            , Solved )
  , Expected = set.from_list( [4,5] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

% todo : make into a lambda, or disappear via currying
:- func gt3( int ) = bool.
gt3( Elt ) = (if Elt > 3 then yes else no).

:- func testQCond = bool.
testQCond = Res :-
  QC = qCond( gt3 )
  , checkNQuery( QC, 5, Res5 )
  , checkNQuery( QC, 3, Res3 )
  , checkNQuery( QC, 3, Res0 )
  , Res = (if [Res5, Res3, Res0] = [yes,no,no] then yes else no).

:- func testQNot = bool.
testQNot = Res :-
    checkNQuery( qNot(1), 1, Res1 )
  , checkNQuery( qNot(1), 0, Res0 )
  , Res = (if [Res1, Res0] = [no,yes] then yes else no).

main(!IO) :-
   io.write_string( "testQElt: "  ++ string(testQElt)  ++ "\n", !IO),
   io.write_string( "testQFind: " ++ string(testQFind) ++ "\n", !IO),
   io.write_string( "testQCond: " ++ string(testQCond) ++ "\n", !IO),
   io.write_string( "testQNot: "  ++ string(testQNot)  ++ "\n", !IO).
