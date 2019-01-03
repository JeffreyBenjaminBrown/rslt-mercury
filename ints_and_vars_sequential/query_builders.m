:- module query_builders.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- import_module query.

% TODO ? Handle mismatched conditions differently?
  % Maybe these (qNot, qIn, qNotIn, more) should throw errors when ...
    % X is not present.
    % X is present but not the requested constructor (foundSet or foundElt).
% TODO ? test mismatches more thoroughly (in testQCond).
  % Currently mismatches are not tested for qIn and qNotIn.

:- func qNot( var, subst, int ) = bool.
:- func qIn( var, subst, int ) = bool.
:- func qNotIn( var, subst, int ) = bool.
:- func qFuncOfVar( func(int) = int, var, list(int), subst ) = list(int).


:- implementation.
:- import_module map.
:- import_module set.

qNot( var(X), subst(M), Int ) = Res :-
  ( if contains( M, var(X) )
    then Found = map.lookup( M, var(X) )
         , ( ( Found = foundSet(_)
             , Res = yes )
           ; ( Found = foundElt( Elt )
             , Res = (if Elt = Int then no else yes ) ) )
    else Res = yes ).

qIn( var(X), subst(M), Int ) = Res :-
  ( if contains( M, var(X) )
    then Found = map.lookup( M, var(X) )
         , ( ( Found = foundElt(_)
             , Res = no )
           ; ( Found = foundSet( Set )
             , Res = (if set.member( Int, Set ) then yes else no ) ) )
    else Res = no ).

qNotIn( var(X), subst(M), Int ) = Res :-
  ( if contains( M, var(X) )
    then Found = map.lookup( M, var(X) )
         , ( ( Found = foundElt(_)
             , Res = yes )
           ; ( Found = foundSet( Set )
             , Res = (if set.member( Int, Set ) then no else yes ) ) )
    else Res = yes ).

qFuncOfVar( F, var(X), Space, subst(M) ) = Res :-
  ( if contains( M, var(X) )
    then Found = map.lookup( M, var(X) )
       , ( ( Found = foundElt( Elt )
           , ( if list.member( Elt, Space )
               then Res = [ F(Elt) ]
               else Res = [] ) )
         ; ( Found = foundSet( _ )
           , Res = [] ) )
    else Res = [] ).
