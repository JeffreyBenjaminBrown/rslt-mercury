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

:- func fiveSpace = list(int).
:- pred eq( X, X, bool ).
:- mode eq( in, in, out ) is det.
:- func eq( X, X) = bool.

:- func testFindable = list(bool).
:- func testAllChecks = list(bool).
:- func testPassesAllChecks = list(bool).
:- func testQElt = list(bool).
:- func testQFind = list(bool).
:- func testQCond = list(bool).
:- func testQAnd = list(bool).
:- func testQOr  = list(bool).

fiveSpace = [1,2,3,4,5].

eq( X, Y, Bool ) :-
  Bool = (if X=Y then yes else no).
eq( X, Y ) = Bool :-
  eq( X, Y, Bool ).

testFindable = [T1, not(T2), T3, not(T4), T5] :-
    QF = qqFind( qFind( list.filter( <(2) ) ) )
  , QC = qqCond( qCond( func( Int ) = (if Int > 4 then no else yes) ) )
  , findable( QF, T1 )
  , findable( QC, T2 )
  , findable( qqAnd( [QF] ), T3 )
  , findable( qqAnd( [QC] ), T4 )
  , findable( qqAnd( [QC, QF] ), T5 ).

testAllChecks = Res :-
    Checks = [ qCond( func( Int ) = (if Int > 2 then yes else no) )
             , qCond( func( Int ) = (if Int < 4 then yes else no) ) ]
  , allChecks( Checks, 1, T1 )
  , allChecks( Checks, 3, T3 )
  , allChecks( Checks, 5, T5 )
  , Res = [ not( bool.and_list(T1) )
          ,      bool.and_list(T3)
          , not( bool.and_list(T5) ) ].

testPassesAllChecks = [ not(T1), T3, not(T5), E7] :-
    Checks = [ qCond( func( Int ) = (if Int > 2 then yes else no) )
             , qCond( func( Int ) = (if Int < 4 then yes else no) ) ]
  , passesAllChecks( Checks, 1, T1 )
  , passesAllChecks( Checks, 3, T3 )
  , passesAllChecks( Checks, 5, T5 )
  , passesAllChecks( [],     7, E7 ).

testQElt = Res :-
    runQFind( fiveSpace, qElt(3), Find3 )
  , runQFind( fiveSpace, qElt(7), Find7 )
  , Res = [ eq( Find3, [3] )
          , eq( Find7, [] ) ].

testQFind = Res :-
    runQFind( fiveSpace, qFind( list.filter( <(3) ) ), Sol3 )
  , runQFind( fiveSpace, qFind( list.filter( <(9) ) ), Sol9 )
  , Res = [ eq( Sol3, [4,5] )
          , eq( Sol9, [] ) ].

testQCond = [ Res5, not(Res3), not(Res0) ] :-
  QC = qCond( func( Int ) = (if Int > 3 then yes else no) )
  , checkQCond( QC, 5, Res5 )
  , checkQCond( QC, 3, Res3 )
  , checkQCond( QC, 0, Res0 ).

:- pred inQueryOverFiveSpace( query, int ).
:- mode inQueryOverFiveSpace( in, out ) is nondet.
inQueryOverFiveSpace( Q, F ) :-
  inQuery( fiveSpace, Q, F ).

testQAnd = Res :-
    QQFgt1 = qqFind( qFind( list.filter( <(1) ) ) )
  , QQFlt5 = qqFind( qFind( list.filter( >(5) ) ) )
  , QQC = qqCond( qCond( func( Int )
                         = (if Int = 3 then no else yes) ) )
  , InQueryOverFiveSpace = ( pred( Q :: in, F :: out ) is nondet :-
                             inQuery( fiveSpace, Q, F ) )

  % TODO Why can't I use InQueryOverFiveSpace
  % instead of inQueryOverFiveSpace for these?
  , solutions( inQueryOverFiveSpace( qqAnd( [QQFgt1             ] ) ) , F1 )
  , solutions( inQueryOverFiveSpace( qqAnd( [        QQFlt5     ] ) ) , F2 )
  , solutions( inQueryOverFiveSpace( qqAnd( [                QQC] ) ) , F3 )
  , solutions( inQueryOverFiveSpace( qqAnd( [QQFgt1, QQFlt5     ] ) ) , F4 )
  , solutions( inQueryOverFiveSpace( qqAnd( [QQFgt1        , QQC] ) ) , F5 )
  , solutions( inQueryOverFiveSpace( qqAnd( [        QQFlt5, QQC] ) ) , F6 )
  , solutions( inQueryOverFiveSpace( qqAnd( [QQFgt1, QQFlt5, QQC] ) ) , F7 )

  , Res = [ eq( F1,           [  2,3,4,5] )
          , eq( F2,           [1,2,3,4  ] )
          , eq( F3,           [         ] )
          , eq( F4,           [  2,3,4  ] )
          , eq( F5,           [  2,  4,5] )
          , eq( F6,           [1,2,  4  ] )
          , eq( F7,           [  2,  4  ] ) ].

testQOr = Res :-
    QQFgt1 = qqFind( qFind( list.filter( <(1) ) ) )
  , QQFlt5 = qqFind( qFind( list.filter( >(5) ) ) )
  , QQC = qqCond( qCond( func( Int )
                         = (if Int = 3 then no else yes) ) )

  , solutions( inQueryOverFiveSpace( qqOr( [QQFgt1             ] ) ) , F1 )
  , solutions( inQueryOverFiveSpace( qqOr( [        QQFlt5     ] ) ) , F2 )
  , solutions( inQueryOverFiveSpace( qqOr( [                QQC] ) ) , F3 )
  , solutions( inQueryOverFiveSpace( qqOr( [QQFgt1, QQFlt5     ] ) ) , F4 )
  , solutions( inQueryOverFiveSpace( qqOr( [QQFgt1        , QQC] ) ) , F5 )
  , solutions( inQueryOverFiveSpace( qqOr( [        QQFlt5, QQC] ) ) , F6 )
  , solutions( inQueryOverFiveSpace( qqOr( [QQFgt1, QQFlt5, QQC] ) ) , F7 )

  , Res = [ eq( F1,           [  2,3,4,5] )
          , eq( F2,           [1,2,3,4  ] )
          , eq( F3,           [         ] )
          , eq( F4,           [1,2,3,4,5] )
          , eq( F5,           [         ] )
          , eq( F6,           [         ] )
          , eq( F7,           [         ] ) ].


:- pred test(string::in, list(bool)::in, io::di, io::uo) is det.
test( Name, Results, !IO ) :-
  io.write_string( Name ++ ": " ++
    string( bool.and_list( Results ) `with_type` bool ) ++ "\n", !IO).

main(!IO) :-
    test( "testFindable", testFindable, !IO)
  , test( "testAllChecks", testAllChecks, !IO)
  , test( "testPassesAllChecks", testPassesAllChecks, !IO )
  , test( "testQElt", testQElt, !IO)
  , test( "testQFind", testQFind, !IO)
  , test( "testQCond", testQCond, !IO)
  , test( "testQAnd", testQAnd, !IO)
  , test( "testQOr", testQOr , !IO)
  .
