:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- type qFind ---> qElt( int )
                 ; qFind( func( list(int) ) = list(int) ).

:- type qCond ---> qCond( func( int ) = bool).

:- type query ---> qFind( qFind ).
%                ; qCond( qCond ).
%                ; qAnd( list(query) )
%                ; qOr( list(query) )
%                ; qVar( string ).

:- pred runQFind( list(int), qFind, list(int) ).
:- mode runQFind( in,        in,    out       ) is det.

:- pred checkQCond( qCond, int, bool ).
:- mode checkQCond( in,     in,  out ) is det.


:- implementation.

checkQCond( qCond(Q) , Elt, Q(Elt) ).

runQFind( List, qElt( Elt ), Res    ) :-
  Res = ( if list.member( Elt, List )
          then [Elt] else [] ).
runQFind( List, qFind( Gen ), Gen( List ) ).
