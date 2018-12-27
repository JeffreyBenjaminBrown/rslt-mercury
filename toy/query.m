:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- type qFind ---> qElt( int )
                 ; qFind( func( list(int) ) = list(int) ).

:- type qCond ---> qCond( func( int ) = bool).

:- type query ---> qFind( qFind )
                 ; qCond( qCond )
                 ; qAnd( list(query) ).
%                ; qOr( list(query) )
%                ; qVar( string ).

:- pred runQuery( list(int), query, list(int) ).
:- mode runQuery( in,        in,    out       ) is semidet.

:- pred runQFind( list(int), qFind, list(int) ).
:- mode runQFind( in,        in,    out       ) is det.

:- pred checkQCond( qCond,  int, bool ).
:- mode checkQCond( in,     in,  out  ) is det.
:- pred checkQCond( qCond,  int ).
:- mode checkQCond( in,     in        ) is semidet.


:- implementation.
:- import_module set.

:- pred findable( query, bool ).
:- mode findable( in, out ) is det.
findable( qFind(_)  , yes ).
findable( qCond(_)  , no ).
findable( qAnd( Qs ), Res ) :-
  list.map( findable, Qs, Findables )
  , Res = bool.and_list( Findables ).

:- pred findable( query ).
:- mode findable( in ) is semidet.
findable( In ) :-
  findable( In, yes ).

:- pred passesAllChecks( list(qCond), int ).
:- mode passesAllChecks( in,          in  ) is semidet.
passesAllChecks( Qs, Elt ) :-
  list.all_true( (pred( Q :: in ) is semidet :- checkQCond( Q, Elt ) )
               , Qs ).

runQuery( Space, qFind( QF ), Res ) :-
  runQFind( Space, QF, Res ).
runQuery( Space, qAnd( Qs ), Res ) :-
  list.filter( findable, Qs, QFs, QNs )
  , list.map( runQuery( Space ), QFs, FoundLists )
  , list.condense( FoundLists, Founds )
%  , list.filter( passesAllChecks( QNs ) % TODO next
%               , Founds
%               , Checkeds )
%  , set.list_to_set( Checkeds, CheckedSets )
  , Res = [].

runQFind( Space, qElt( Elt ),  Res          ) :-
  Res = ( if list.member( Elt, Space )
          then [Elt] else [] ).
runQFind( Space, qFind( Gen ), Gen( Space ) ).

checkQCond(    qCond(Q), Elt, Q(Elt) ).
checkQCond(    qCond(Q), Elt ) :-
  checkQCond(  qCond(Q), Elt, Q(Elt) ).
