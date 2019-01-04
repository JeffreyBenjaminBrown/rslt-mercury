:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.

:- type program ---> program( list(int), list(var), list((var,query)) ).
:- type subst ---> subst( substElts :: map( var,     int  )
                        , substSets :: map( var, set(int) ) ).
:- type var ---> var( string ).

:- type qSearch ---> qElt( int )
  ; qSearch( search :: func( list(int), subst ) = list(int)
     , searchEltDeps :: set( var )
     , searchSetDeps :: set( var ) ).
:- type qCond --->
    qCond(   cond   :: func(            subst,         int ) = bool
     , condEltDeps   :: set( var )
     , condSetDeps   :: set( var ) ).
:- type query ---> qqSearch( qSearch )
                 ; qqCond( qCond )
                 ; qqAnd( list(query) )
                 ; qqOr( list(query) ).

:- func allKeysInMap( map(K,V), set(K) ) = bool.

:- pred searchable( subst, query, bool ).
:- mode searchable( in,    in,    out  ) is det.
:- pred searchable( subst, query       ).
:- mode searchable( in,    in          ) is semidet.

%:- pred checkQCond( qCond,  subst, int, bool ).
%:- mode checkQCond( in,     in,    in,  out  ) is det.
%:- pred checkQCond( qCond,  subst, int       ).
%:- mode checkQCond( in,     in,    in        ) is semidet.
%
%:- pred allChecks( list(qCond), subst, int, list(bool) ).
%:- mode allChecks( in,          in,    in,  out        ) is det.
%
%:- pred passesAllChecks( list(qCond), subst, int, bool ) is det.
%:- mode passesAllChecks( in,          in,    in,  out  ) is det.
%:- pred passesAllChecks( list(qCond), subst, int       ).
%:- mode passesAllChecks( in,          in,    in        ) is semidet.
%
%:- pred runQSearch( list(int), subst, qSearch, list(int) ).
%:- mode runQSearch( in,        in,    in,      out       ) is det.
%:- pred inQSearch(  list(int), subst, qSearch, int  ).
%:- mode inQSearch(  in,        in,    in,      in   ) is semidet.
%:- mode inQSearch(  in,        in,    in,      out  ) is nondet.
%
%:- pred runQuery( list(int), subst, query, list(int) ).
%:- mode runQuery( in,        in,    in,    out       ) is semidet.
%:- pred inQuery(  list(int), subst, query, int ).
%:- mode inQuery(  in,        in,    in,    out       ) is nondet.
%:- mode inQuery(  in,        in,    in,    in        ) is semidet.


:- implementation.

allKeysInMap( Map, KSet ) = Res :-
    Memberships = set.map( func( In ) = Bool :-
                           set.is_member( In, set( map.keys( Map ) ), Bool )
                         , KSet )
  , Res = bool.and_list( set.to_sorted_list( Memberships ) ).

searchable( _, qqSearch( qElt(_) ), yes ).
searchable( subst( Elts, Sets)
          , qqSearch( qSearch( _, EltDeps, SetDeps ) )
          , Bool ) :-
  Bool =       allKeysInMap( Elts, EltDeps )
         `and` allKeysInMap( Sets, SetDeps ).
searchable( subst( Elts, Sets)
          , qqCond(   qCond(   _, EltDeps, SetDeps ) )
          , Bool ) :-
  Bool =       allKeysInMap( Elts, EltDeps )
         `and` allKeysInMap( Sets, SetDeps ).
searchable( Subst, qqAnd( Qs ), Res ) :-
  list.map( searchable(Subst), Qs, Searchables )
  , Res = bool.or_list( Searchables ).
searchable( Subst, qqOr( Qs ),  Res ) :-
  list.map( searchable(Subst), Qs, Searchables )
  , Res = bool.and_list( Searchables ).

searchable(   Subst, In ) :-
  searchable( Subst, In, yes ).

%checkQCond(    qCond(C,_), Subst, Elt, C(Subst, Elt) ).
%checkQCond(    QC,         Subst, Elt ) :-
%  checkQCond(  QC,         Subst, Elt, yes ).
%
%allChecks( QCs, Subst, Elt, Results ) :-
%  list.map( pred( QC :: in, Bool :: out ) is det :-
%              checkQCond( QC, Subst, Elt, Bool )
%          , QCs, Results ).
%
%passesAllChecks( Cs, Subst, Elt, Res ) :-
%  allChecks( Cs, Subst, Elt, AllChecks )
%  , Res = ( if list.all_true( pred( Bool :: in ) is semidet :- Bool = yes
%                            , AllChecks )
%            then yes else no ).
%passesAllChecks( Cs, Subst, Elt ) :-
%  passesAllChecks( Cs, Subst, Elt, yes ).
%
%runQSearch( Space, _, qElt( Elt ),  Res          ) :-
%  Res = ( if list.member( Elt, Space )
%          then [Elt] else [] ).
%runQSearch( Space, Subst, qSearch( Gen, _ ), Gen( Space, Subst ) ).
%inQSearch(    Space, Subst, Q,  Elt ) :-
%  runQSearch( Space, Subst, Q, Elts )
%  , list.member( Elt, Elts ).
%
%runQuery( Space, Subst, qqSearch( QF ), Res ) :-
%  runQSearch( Space, Subst, QF, Res ).
%
%runQuery( Space, Subst, qqAnd( Qs ), Checkeds ) :-
%  list.filter( searchable, Qs, QQFs, QQCs )
%  , list.map( runQuery( Space, Subst ), QQFs, FoundLists )
%  , list.map( set.list_to_set, FoundLists, FoundSets )
%  , Founds = set.to_sorted_list( set.intersect_list( FoundSets ) )
%  , list.map( pred( qqCond( QC ) :: in, QC :: out ) is semidet
%            , QQCs, QCs )
%  , list.filter( passesAllChecks( QCs, Subst ), Founds, Checkeds ).
%
%runQuery( Space, Subst, qqOr( Qs ), Founds ) :-
%    searchable( qqOr( Qs ) )
%  , list.filter( searchable, Qs, QQFs, _ ) % TODO Use the qqConds in the _
%  , list.map( runQuery( Space, Subst ), QQFs, FoundLists )
%  , list.map( set.list_to_set, FoundLists, FoundSets )
%  , Founds = set.to_sorted_list( set.union_list( FoundSets ) ).
%
%inQuery( Space, Subst, Q, Elt ) :-
%  runQuery( Space, Subst, Q, Found )
%  , list.member( Elt, Found ).
