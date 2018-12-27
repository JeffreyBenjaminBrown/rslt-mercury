:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- type qFind ---> qElt( int )
                 ; qFind( func( list(int) ) = list(int) ).

:- type qCond ---> qCond( func( int ) = bool).

:- type query ---> qFind( qFind ). % TODO next
%                ; qCond( qCond ).
%                ; qAnd( list(query) )
%                ; qOr( list(query) )
%                ; qVar( string ).

:- pred runQFind( list(int), qFind, list(int) ).
:- mode runQFind( in,        in,    out       ) is det.

:- pred checkQCond( qCond, int, bool ).
:- mode checkQCond( in,     in,  out ) is det.

%:- pred runQuery( list(int), query, list(int) ).
%:- mode runQuery( in,        in,    out       ) is det.


:- implementation.

runQFind( Space, qElt( Elt ),  Res          ) :-
  Res = ( if list.member( Elt, Space )
          then [Elt] else [] ).
runQFind( Space, qFind( Gen ), Gen( Space ) ).

checkQCond( qCond(Q) , Elt, Q(Elt) ).

