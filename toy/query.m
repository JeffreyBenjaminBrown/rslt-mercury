:- module query.
:- interface.
:- import_module bool.
:- import_module int.
:- import_module list.

:- type pQuery ---> qElt( int )
      ; qFind( func( list(int) ) = list(int) ).

:- type nQuery ---> qNot( int )
      ; qCond( func( int ) = bool).

:- pred runPQuery( list(int), pQuery, list(int) ).
:- mode runPQuery( in,        in,     out       ) is det.

:- pred checkNQuery( nQuery, int, bool ).
:- mode checkNQuery( in,     in,  out ) is det.


:- implementation.

checkNQuery( qCond(Q) , Elt, Q(Elt) ).
checkNQuery( qNot(Int), Elt, Res ) :-
  Res = (if ( Elt \= Int )
        then yes else no).

runPQuery( List, qElt( Elt ), Res    ) :-
  Res = ( if list.member( Elt, List )
          then [Elt] else [] ).
runPQuery( List, qFind( Gen ), Gen( List ) ).
