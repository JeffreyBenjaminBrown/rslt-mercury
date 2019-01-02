:- module queries_not.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module query.

:- func qNot( var, subst, int ) = bool.

:- implementation.

qNot( var(X), subst(M), Int ) = Res :-
  ( if contains( M, var(X) )
    then Found = map.lookup( M, var(X) )
         , ( ( Found = foundSet(_)
             , Res = yes )
           ; ( Found = foundElt( Elt )
             , Res = (if Elt = Int then no else yes ) ) )
    else Res = yes ).
