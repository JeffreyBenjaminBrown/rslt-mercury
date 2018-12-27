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
  runQFind( fiveNumberSpace
          , qElt( 3 )
          , Solved )
  , Expected = set.from_list( [3] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

:- func testQFind = bool.
testQFind = Res :-
  runQFind( fiveNumberSpace
          , qFind( list.filter( <(3) ) )
          , Solved )
  , Expected = set.from_list( [4,5] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

:- func testQCond = bool.
testQCond = Res :-
  QC = qCond( func( Int ) = (if Int > 3 then yes else no) )
  , checkQCond( QC, 5, Res5 )
  , checkQCond( QC, 3, Res3 )
  , checkQCond( QC, 3, Res0 )
  , Res = (if [Res5, Res3, Res0] = [yes,no,no] then yes else no).

main(!IO) :-
   io.write_string( "testQElt: "  ++ string(testQElt)  ++ "\n", !IO),
   io.write_string( "testQFind: " ++ string(testQFind) ++ "\n", !IO),
   io.write_string( "testQCond: " ++ string(testQCond) ++ "\n", !IO).
