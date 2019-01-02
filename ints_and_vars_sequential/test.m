:- module test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.


:- implementation.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module query.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- import_module util.

:- pred test(string::in, list(bool)::in, io::di, io::uo) is det.
:- func fiveSpace = list(int).

:- func testSearchable = list(bool).
:- func testQCond = list(bool).

test( Name, Results, !IO ) :-
  io.write_string( Name ++ ": " ++
    string( bool.and_list( Results ) `with_type` bool ) ++ "\n", !IO).

fiveSpace = [1,2,3,4,5].

testSearchable = [ S1, not(S2)
                 , T1, not(T2), T3, not(T4), T5] :-
    QF = qqSearch( qSearch( func(_, IntList) = Res :-
                            list.filter( <(2), IntList, Res ) ) )
  , QC = qqCond( qCond( func(_, Int ) = (if Int > 4 then no else yes) ) )
  , S1 = (if searchable( QF ) then yes else no)
  , S2 = (if searchable( QC ) then yes else no)
  , searchable( QF               , T1 )
  , searchable( QC               , T2 )
  , searchable( qqAnd( [QF] )    , T3 )
  , searchable( qqAnd( [QC] )    , T4 )
  , searchable( qqAnd( [QC, QF] ), T5 ).

testQCond = [ Res5gt3, not(Res3gt3), not(Res0gt3)
            , Res3notEmpty, Res3NotAbsent
            , not(Res3Not3), Res3Not4, Res3NotSet ] :-
  % QCgt3 ignores the Subst argument
    QCgt3 = qCond( func( _, Int ) = (if Int > 3 then yes else no) )
  % QCNotX checks against var('X'), if present, in the Subst
  , QCnotX = qCond( func( subst(M), Int ) = Res :-
                  ( if contains( M, var("X") )
                    then Found = map.lookup( M, var("X"))
                         , ( ( Found = foundSet(_)
                             , Res = yes )
                           ; ( Found = foundElt( Elt )
                             , Res = (if Elt = Int then no else yes) ) )
                    else Res = yes ) )
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
  , checkQCond( QCnotX
      , subst( map.singleton( var("X"), foundSet( set.from_list( [3,4] )) ) )
      , 3, Res3NotSet ).

main(!IO) :-
    test( "testSearchable", testSearchable, !IO)
  , test( "testQCond", testQCond, !IO )
  .
