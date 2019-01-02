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

% % "lambdas" for tests must be defined at the top level,
% % because lambdas cannot be curried.
%
%:- pred inQueryOverFiveSpace( query, int ).
%:- mode inQueryOverFiveSpace( in, out ) is nondet.
%inQueryOverFiveSpace( Q, F ) :-
%  inQuery( fiveSpace, Q, F ).
%testQAnd = Res :-
%    QQFgt1 = qqFind( qFind( list.filter( <(1) ) ) )
%  , solutions( inQueryOverFiveSpace( qqAnd( [QQFgt1             ] ) ), F1 )
%  , Res = [ eq( F1,           [  2,3,4,5] ) ].

main(!IO) :-
    test( "testSearchable", testSearchable, !IO)
  .
