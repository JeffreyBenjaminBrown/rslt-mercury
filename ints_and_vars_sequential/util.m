:- module util.
:- interface.
:- import_module bool.

:- pred eq( T, T, bool ).
:- mode eq( in, in, out ) is det.
:- func eq( T, T ) = bool.


:- implementation.

eq( X, Y, Bool ) :-
  Bool = (if X=Y then yes else no).
eq( X, Y ) = Bool :-
  eq( X, Y, Bool ).
