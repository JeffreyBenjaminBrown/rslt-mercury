:- module build_queries.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- import_module query.

:- func qNotElt( var, subst, int ) = bool.
:- func qIn(     var, subst, int ) = bool.
:- func qNotIn(  var, subst, int ) = bool.

:- func qFuncOfVar( func(int) = int
                  , var, list(int), subst )  
  = list(int).


:- implementation.
:- import_module map.
:- import_module set.

qNotElt( V, subst(SubstElts,_), Int ) = Res :-
  ( if contains( SubstElts, V )
    then Found = map.lookup( SubstElts, V )
         , Res = (if Int = Found then no else yes)
    else Res = no ). % TODO ? throw an error instead. similar below.

qIn( V, subst(_,SubstSets), Int ) = Res :-
  ( if contains( SubstSets, V )
    then Found = map.lookup( SubstSets, V )
         , Res = (if set.member( Int, Found ) then yes else no)
    else Res = no ).

qNotIn( V, subst(_,SubstSets), Int ) = Res :-
  ( if contains( SubstSets, V )
    then Found = map.lookup( SubstSets, V )
         , Res = (if set.member( Int, Found ) then no else yes)
    else Res = no ).

qFuncOfVar( F, V, Space, subst( SubstElts, _ ) ) = Res :-
  ( if contains( SubstElts, V )
    then Found = map.lookup( SubstElts, V )
         , Mapped = F(Found)
         , Res = (if list.member( Mapped, Space ) then [Mapped] else [] )
    else Res = [] ).
