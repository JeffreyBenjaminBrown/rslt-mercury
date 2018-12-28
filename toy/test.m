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

:- func testFindable = list(bool).
:- func testAllChecks = list(bool).
:- func testPassesAllChecks = list(bool).
:- func testQElt = list(bool).
:- func testQFind = list(bool).
:- func testQCond = list(bool).
:- func testQAnd = list(bool).

fiveNumberSpace = [1,2,3,4,5].

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
    runQFind( fiveNumberSpace, qElt(3), Find3 )
  , runQFind( fiveNumberSpace, qElt(7), Find7 )
  , Res = [ eq( Find3, [3] )
          , eq( Find7, [] ) ].

testQFind = Res :-
    runQFind( fiveNumberSpace, qFind( list.filter( <(3) ) ), Sol3 )
  , runQFind( fiveNumberSpace, qFind( list.filter( <(9) ) ), Sol9 )
  , Res = [ eq( Sol3, [4,5] )
          , eq( Sol9, [] ) ].

testQCond = [ Res5, not(Res3), not(Res0) ] :-
  QC = qCond( func( Int ) = (if Int > 3 then yes else no) )
  , checkQCond( QC, 5, Res5 )
  , checkQCond( QC, 3, Res3 )
  , checkQCond( QC, 0, Res0 ).

testQAnd = Res :-
    QQF = qqFind( qFind( list.filter( <(3) ) ) )
  , QQC = qqCond( qCond( func( Int )
                         = (if Int > 4 then no else yes) ) )
  % TODO ? How can I factor a single two-parameter pred
  % (of the form pred( Q :: in, F :: out )) out of the multiple preds below?
  , solutions( pred( F :: out ) is nondet :-
               inQuery( fiveNumberSpace, qqAnd( [QQF     ] ), F )
             , F1 )
  , solutions( pred( F :: out ) is nondet :-
               inQuery( fiveNumberSpace, qqAnd( [QQF, QQC] ), F )
             , F2 )
  , solutions( pred( F :: out ) is nondet :-
               inQuery( fiveNumberSpace, qqAnd( [     QQC] ), F )
             , F3 )
  , Res = [ eq( F1, [4,5] )
          , eq( F2, [4]   )
          , eq( F3, []    ) ].

:- func showBool( bool ) = string. % TODO ? Why do I need this?
showBool( B ) = string(B).

:- pred test(string::in, list(bool)::in, io::di, io::uo) is det.
test( Name, Results, !IO ) :-
  io.write_string( Name ++ ": " ++
    showBool( bool.and_list( Results ) ) ++ "\n", !IO).

main(!IO) :-
    test( "testFindable", testFindable, !IO)
  , test( "testAllChecks", testAllChecks, !IO)
  , test( "testPassesAllChecks", testPassesAllChecks, !IO )
  , test( "testQElt", testQElt, !IO)
  , test( "testQFind", testQFind, !IO)
  , test( "testQCond", testQCond, !IO)
  , test( "testQAnd", testQAnd, !IO)
  .
