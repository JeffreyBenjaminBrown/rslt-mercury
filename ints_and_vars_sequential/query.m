:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.

:- type program ---> program( list(var), list(query) ).
:- type subst ---> subst( map( var, found ) ).
:- type var ---> var( string ).
:- type found ---> foundElt( int )
                 ; foundSet( set(int) ).

:- type qSearch ---> qElt( int )
                   ; qSearch( func( list(int), subst ) = list( int ) ).
:- type qCond ---> qCond(     func(            subst,          int ) = bool ).
:- type query ---> qqSearch( qSearch )
                 ; qqCond( qCond )
                 ; qqAnd( list(query) )
                 ; qqOr( list(query) ).


:- pred searchable( query, bool ).
:- mode searchable( in,    out  ) is det.
:- pred searchable( query       ).
:- mode searchable( in          ) is semidet.

:- pred checkQCond( qCond,  subst, int, bool ).
:- mode checkQCond( in,     in,    in,  out  ) is det.
:- pred checkQCond( qCond,  subst, int       ).
:- mode checkQCond( in,     in,    in        ) is semidet.

:- pred allChecks( list(qCond), subst, int, list(bool) ).
:- mode allChecks( in,          in,    in,  out        ) is det.

:- pred passesAllChecks( list(qCond), subst, int, bool ) is det.
:- mode passesAllChecks( in,          in,    in,  out  ) is det.
:- pred passesAllChecks( list(qCond), subst, int       ).
:- mode passesAllChecks( in,          in,    in        ) is semidet.

:- pred runQSearch( list(int), subst, qSearch, list(int) ).
:- mode runQSearch( in,        in,    in,      out       ) is det.
:- pred inQSearch(  list(int), subst, qSearch, int  ).
:- mode inQSearch(  in,        in,    in,      in   ) is semidet.
:- mode inQSearch(  in,        in,    in,      out  ) is nondet.

%:- pred runQuery( list(int), query, list(int) ).
%:- mode runQuery( in,        in,    out       ) is semidet.
%:- pred inQuery(  list(int), query, int ).
%:- mode inQuery(  in,        in,    out       ) is nondet.
%:- mode inQuery(  in,        in,    in        ) is semidet.


:- implementation.

searchable( qqSearch(_)  , yes ).
searchable( qqCond(_)  , no ).
searchable( qqAnd( Qs ), Res ) :-
  list.map( searchable, Qs, Searchables )
  , Res = bool.or_list( Searchables ).
searchable( qqOr( Qs ),  Res ) :-
  list.map( searchable, Qs, Searchables )
  , Res = bool.and_list( Searchables ).
searchable( In ) :-
  searchable( In, yes ).

checkQCond(    qCond(C), Subst, Elt, C(Subst, Elt) ).
checkQCond(    qCond(C), Subst, Elt ) :-
  checkQCond(  qCond(C), Subst, Elt, yes ).

allChecks( Cs, Subst, Elt, Results ) :-
  list.map( pred( C :: in, Bool :: out ) is det :-
              checkQCond( C, Subst, Elt, Bool )
          , Cs, Results ).

passesAllChecks( Cs, Subst, Elt, Res ) :-
  allChecks( Cs, Subst, Elt, AllChecks )
  , Res = ( if list.all_true( pred( Bool :: in ) is semidet :- Bool = yes
                            , AllChecks )
            then yes else no ).
passesAllChecks( Cs, Subst, Elt ) :-
  passesAllChecks( Cs, Subst, Elt, yes ).

runQSearch( Space, _, qElt( Elt ),  Res          ) :-
  Res = ( if list.member( Elt, Space )
          then [Elt] else [] ).
runQSearch( Space, Subst, qSearch( Gen ), Gen( Space, Subst ) ).
inQSearch(    Space, Subst, Q,  Elt ) :-
  runQSearch( Space, Subst, Q, Elts )
  , list.member( Elt, Elts ).

%runQuery( Space, qqSearch( QF ), Res ) :-
%  runQSearch( Space, QF, Res ).
%runQuery( Space, qqAnd( Qs ), Checkeds ) :-
%  list.filter( searchable, Qs, QQFs, QQCs )
%  , list.map( runQuery( Space ), QQFs, FoundLists )
%  , list.map( set.list_to_set, FoundLists, FoundSets )
%  , Founds = set.to_sorted_list( set.intersect_list( FoundSets ) )
%  , list.map( ( pred( qqCond( QC ) :: in, QC :: out ) is semidet )
%            , QQCs, QCs )
%  , list.filter( passesAllChecks(QCs), Founds, Checkeds ).
%runQuery( Space, qqOr( Qs ), Founds ) :-
%    searchable( qqOr( Qs ) )
%  , list.filter( searchable, Qs, QQFs, _ ) % TODO Use the qqConds in the _
%  , list.map( runQuery( Space ), QQFs, FoundLists )
%  , list.map( set.list_to_set, FoundLists, FoundSets )
%  , Founds = set.to_sorted_list( set.union_list( FoundSets ) ).
%inQuery( Space, Q, Elt ) :-
%  runQuery( Space, Q, Found )
%  , list.member( Elt, Found ).
