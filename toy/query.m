:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- type qFind ---> qElt( int )
                 ; qFind( func( list(int) ) = list(int) ).

:- type qCond ---> qCond( func( int ) = bool).

:- type query ---> qqFind( qFind )
                 ; qqCond( qCond )
                 ; qqAnd( list(query) ).
%                ; qqOr( list(query) )
%                ; qqVar( string ).

:- pred findable( query, bool ).
:- mode findable( in, out ) is det.
:- pred findable( query ).
:- mode findable( in ) is semidet.

:- pred allChecks( list(qCond), int, list(bool) ).
:- mode allChecks( in,          in,  out        ) is det.

:- pred passesAllChecks( list(qCond), int       ).
:- mode passesAllChecks( in,          in        ) is semidet.
:- pred passesAllChecks( list(qCond), int, bool ) is det.
:- mode passesAllChecks( in,          in,  out  ) is det.

:- pred runQuery( list(int), query, list(int) ).
:- mode runQuery( in,        in,    out       ) is semidet.
:- pred inQuery(  list(int), query, int ).
:- mode inQuery(  in,        in,    out       ) is nondet.
:- mode inQuery(  in,        in,    in        ) is semidet.

:- pred runQFind( list(int), qFind, list(int) ).
:- mode runQFind( in,        in,    out       ) is det.
:- pred inQFind(  list(int), qFind, int  ).
:- mode inQFind(  in,        in,    in   ) is semidet.
:- mode inQFind(  in,        in,    out  ) is nondet.

:- pred checkQCond( qCond,  int, bool ).
:- mode checkQCond( in,     in,  out  ) is det.
:- pred checkQCond( qCond,  int ).
:- mode checkQCond( in,     in        ) is semidet.


:- implementation.
:- import_module set.

findable( qqFind(_)  , yes ).
findable( qqCond(_)  , no ).
findable( qqAnd( Qs ), Res ) :-
  list.map( findable, Qs, Findables )
  , Res = bool.or_list( Findables ).
findable( In ) :-
  findable( In, yes ).

allChecks( Cs, Elt, Results ) :-
  list.map( pred( Q :: in, Bool :: out ) is det :-
              checkQCond( Q, Elt, Bool )
          , Cs, Results ).

passesAllChecks( Cs, Elt, Res ) :-
  allChecks( Cs, Elt, AllChecks )
  , Res = ( if list.all_true( pred( Bool :: in ) is semidet :- Bool = yes
                            , AllChecks )
            then yes else no ).
passesAllChecks( Qs, Elt ) :-
  passesAllChecks( Qs, Elt, yes ).

runQuery( Space, qqFind( QF ), Res ) :-
  runQFind( Space, QF, Res ).
runQuery( Space, qqAnd( Qs ), Checkeds ) :-
  list.filter( findable, Qs, QQFs, QQCs )
  , list.map( runQuery( Space ), QQFs, FoundLists )
  , list.condense( FoundLists, Founds ) % todo ? unique
  , list.map( ( pred( qqCond( QC ) :: in, QC :: out ) is semidet )
            , QQCs, QCs )
  , list.filter( passesAllChecks( QCs )
               , Founds
               , Checkeds ).

inQuery( Space, qqFind( QF ), Elt ) :-
  inQFind( Space, QF, Elt ).

runQFind( Space, qElt( Elt ),  Res          ) :-
  Res = ( if list.member( Elt, Space )
          then [Elt] else [] ).
runQFind( Space, qFind( Gen ), Gen( Space ) ).

inQFind( Space, Q,  Elt ) :-
  runQFind( Space, Q, Elts )
  , list.member( Elt, Elts ).

checkQCond(    qCond(C), Elt, C(Elt) ).
checkQCond(    qCond(C), Elt ) :-
  checkQCond(  qCond(C), Elt, C(Elt) ).
