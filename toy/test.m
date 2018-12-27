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
:- pred eq( X, X, bool ).
:- mode eq( in, in, out ) is det.
:- func eq( X, X) = bool.

:- func testFindable = bool.
:- func testQElt = bool.
:- func testQFind = bool.
:- func testQCond = bool.
:- func testQAnd = list(int).

fiveNumberSpace = [1,2,3,4,5].

eq( X, Y, Bool ) :-
  Bool = (if X=Y then yes else no).
eq( X, Y ) = Bool :-
  eq( X, Y, Bool ).

testFindable = Res :-
  QF = qqFind( qFind( list.filter( <(2) ) ) )
  , QC = qqCond( qCond( func( Int ) = (if Int > 4 then no else yes) ) )
  , findable( QF, T1 )
  , findable( QC, T2 )
  , findable( qqAnd( [QF] ), T3 )
  , findable( qqAnd( [QC] ), T4 )
  , findable( qqAnd( [QC, QF] ), T5 )
  , Res = bool.and_list( [ eq( T1, yes )
                         , eq( T2, no )
                         , eq( T3, yes )
                         , eq( T4, no )
                         , eq( T5, yes ) ] ).

testQElt = Res :-
  runQFind( fiveNumberSpace
          , qElt( 3 )
          , Solved )
  , Expected = set.from_list( [3] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

testQFind = Res :-
  runQFind( fiveNumberSpace
          , qFind( list.filter( <(3) ) )
          , Solved )
  , Expected = set.from_list( [4,5] )
  , Res = ( if set.from_list(Solved) = Expected
            then yes else no ).

testQCond = Res :-
  QC = qCond( func( Int ) = (if Int > 3 then yes else no) )
  , checkQCond( QC, 5, Res5 )
  , checkQCond( QC, 3, Res3 )
  , checkQCond( QC, 0, Res0 )
  , Res = (if [Res5, Res3, Res0] = [yes,no,no] then yes else no).

testQAnd = Res :-
  Qs = [ qqFind( qFind( list.filter( <(2) ) ) )
%       , qqCond( qCond( func( Int ) = (if Int > 4 then no else yes) ) )
       ]
  , solutions( pred( F :: out ) is nondet :-
                 inQuery( fiveNumberSpace, qqAnd( Qs ), F )
             , Res ).
%  , Res = ( if set.from_list( Found ) = set.from_list( [3,4] )
%            then yes else no ).

main(!IO) :-
   io.write_string( "testFindable: " ++ string(testFindable) ++ "\n", !IO),
   io.write_string( "testQElt: "     ++ string(testQElt)     ++ "\n", !IO),
   io.write_string( "testQFind: "    ++ string(testQFind)    ++ "\n", !IO),
   io.write_string( "testQCond: "    ++ string(testQCond)    ++ "\n", !IO),
   io.write_string( "testQAnd: "     ++ string(testQAnd)     ++ "\n", !IO).
