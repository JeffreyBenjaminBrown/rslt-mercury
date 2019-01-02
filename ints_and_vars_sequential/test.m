:- module test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.


:- implementation.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module query.
:- import_module queries_not.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- import_module util.

:- pred test(string::in, list(bool)::in, io::di, io::uo) is det.
:- func fiveSpace = list(int).

:- func testSearchable = list(bool).
:- func testQCond = list(bool).
:- func testAllChecks = list(bool).
:- func testPassesAllChecks = list(bool).
:- func testQElt = list(bool).
:- func testQSearch = list(bool).

test( Name, Results, !IO ) :-
  io.write_string( Name ++ ": " ++
    string( bool.and_list( Results ) `with_type` bool ) ++ "\n", !IO).

fiveSpace = [1,2,3,4,5].

testSearchable = [ S1, not(S2)
                 , T1, not(T2), T3, not(T4), T5] :-
    QF = qqSearch( qSearch( func( IntList, _ ) = Res :-
                            list.filter( <(2), IntList, Res ) ) )
  , QC = qqCond( qCond( func(_, Int ) = (if Int > 4 then no else yes) 
                      , set.init ) )
  , S1 = (if searchable( QF ) then yes else no)
  , S2 = (if searchable( QC ) then yes else no)
  , searchable( QF               , T1 )
  , searchable( QC               , T2 )
  , searchable( qqAnd( [QF] )    , T3 )
  , searchable( qqAnd( [QC] )    , T4 )
  , searchable( qqAnd( [QC, QF] ), T5 ).

testQCond = [ Res5gt3, not(Res3gt3), not(Res0gt3)
            , Res3notEmpty, Res3NotAbsent
            , not(Res3Not3), Res3Not4, Res3NotSet
            , not(Res3NotIn34), Res3NotIn45
            , Res3In34, not(Res3In45) ] :-

    QCgt3 = qCond( func( _, Int ) = (if Int > 3 then yes else no), set.init )
  , QCnotX =   qCond( qNot( var("X") )  , set( [ var("X") ] ) )
  , QCnotInX = qCond( qNotIn( var("X") ), set( [ var("X") ] ) )
  , QCInX =    qCond( qIn( var("X") ),    set( [ var("X") ] ) )

  , checkQCond( QCgt3,  subst( map.init ), 5, Res5gt3 )
  , checkQCond( QCgt3,  subst( map.init ), 3, Res3gt3 )
  , checkQCond( QCgt3,  subst( map.init ), 0, Res0gt3 )

  , checkQCond( QCnotX, subst( map.init ), 3, Res3notEmpty )
  , checkQCond( QCnotX, subst( map.singleton( var("Y"), foundElt(3) ) )
              , 3, Res3NotAbsent )
  , checkQCond( QCnotX, subst( map.singleton( var("X"), foundElt(3) ) )
              , 3, Res3Not3 )
  , checkQCond( QCnotX, subst( map.singleton( var("X"), foundElt(4) ) )
              , 3, Res3Not4 )
  , checkQCond( QCnotX, subst( map.singleton( var("X")
                             , foundSet( set.from_list( [3,4] )) ) )
              , 3, Res3NotSet )

  , checkQCond( QCnotInX, subst( map.singleton( var("X")
                               , foundSet( set.from_list( [3,4] ) ) ) )
              , 3, Res3NotIn34 )
  , checkQCond( QCnotInX, subst( map.singleton( var("X")
                               , foundSet( set.from_list( [4,5] ) ) ) )
              , 3, Res3NotIn45 )

  , checkQCond( QCInX, subst( map.singleton( var("X")
                               , foundSet( set.from_list( [3,4] ) ) ) )
              , 3, Res3In34 )
  , checkQCond( QCInX, subst( map.singleton( var("X")
                               , foundSet( set.from_list( [4,5] ) ) ) )
              , 3, Res3In45 ).

testAllChecks = Res :-
    Checks = [ qCond( func( _, Int ) = (if Int > 1 then yes else no)
                    , set.init )
             , qCond( func( _, Int ) = (if Int < 5 then yes else no)
                    , set.init )
             , qCond( qNot( var("X") )
                    , set( [ var("X") ] ) ) ]
  , allChecks( Checks, subst( map.init ), 1, T1 )
  , allChecks( Checks, subst( map.init ), 3, T3 )
  , allChecks( Checks, subst( map.init ), 5, T5 )
  , allChecks( Checks, subst( map.singleton( var("X"), foundElt(3) ) )
             , 3, T3X3 )
  , allChecks( Checks, subst( map.singleton( var("X"), foundElt(4) ) )
             , 3, T3X4 )
  , allChecks( Checks, subst( map.singleton( var("Y"), foundElt(3) ) )
             , 3, T3Y3 )
  , Res = [ not( bool.and_list(T1   ) )
          ,      bool.and_list(T3   )
          , not( bool.and_list(T5   ) )
          , not( bool.and_list(T3X3 ) )
          ,      bool.and_list(T3Y3 )
          ,      bool.and_list(T3X4 ) ].

testPassesAllChecks = Res :-
    Checks = [ qCond( func( _, Int ) = (if Int > 0 then yes else no)
                    , set.init )
             , qCond( qNot( var("X") )
                    , set( [ var("X") ] ) )
             , qCond( qIn( var("Y") )
                    , set( [ var("Y") ] ) ) ]
  , SomeEvens = set.from_list( [0,2,4,6,8,10] )
  , Subst = subst( map.from_assoc_list(
             [ ( var("X") - foundElt(4) )
             , ( var("Y") - foundSet( SomeEvens ) ) ] ) )
  , passesAllChecks( Checks, Subst,             0, T0 )
  , passesAllChecks( Checks, Subst,             5, T5 )
  , passesAllChecks( Checks, Subst,             6, T6 )
  , passesAllChecks( Checks, subst( map.init ), 6, TEmpty6 )
  , Res = [ not(T0), not(T5), T6, not(TEmpty6) ].

testQElt = Res :-
    runQSearch( fiveSpace, subst( map.init ), qElt(3), Search3 )
  , runQSearch( fiveSpace, subst( map.init ), qElt(7), Search7 )
  , Res = [ eq( Search3, [3] )
          , eq( Search7, [] ) ].

testQSearch = Res :-
    runQSearch( fiveSpace, subst( map.init )
              , qSearch( func( In, _ ) = Out :-
                         list.filter( <(3), In, Out ) )
              , Sol3 )
  , runQSearch( fiveSpace, subst( map.init )
              , qSearch( func( In, _ ) = Out :-
                         list.filter( <(9), In, Out ) )
              , Sol9 )
  , runQSearch( fiveSpace
              , subst( map.singleton( var("X"), foundElt( 3 ) ) )
              , qSearch( qFuncOfVar( func(X) = X+3, var("X") ) )
              , SolId )
  , Res = [ eq( Sol3, [4,5] )
          , eq( Sol9, [] ) 
          , eq( SolId, [6] ) ].

main(!IO) :-
    test( "testSearchable", testSearchable, !IO )
  , test( "testQCond", testQCond, !IO )
  , test( "testAllChecks", testAllChecks, !IO )
  , test( "testPassesAllChecks", testPassesAllChecks, !IO )
  , test( "testQElt", testQElt, !IO)
  , test( "testQSearch", testQSearch, !IO )
  .
