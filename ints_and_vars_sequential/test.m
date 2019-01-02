:- module test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.


:- implementation.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module query.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- import_module util.

:- pred test(string::in, list(bool)::in, io::di, io::uo) is det.
:- func fiveSpace = list(int).

:- func nullTest = list(bool).

test( Name, Results, !IO ) :-
  io.write_string( Name ++ ": " ++
    string( bool.and_list( Results ) `with_type` bool ) ++ "\n", !IO).

fiveSpace = [1,2,3,4,5].

nullTest = [yes,yes].

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
    test( "null test", nullTest, !IO)
  .
