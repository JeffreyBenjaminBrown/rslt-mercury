:- module wishes.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module set.
:- import_module solutions.
:- import_module string.

:- pred firstTen( int, bool ).
:- mode firstTen( in,  out ) is det.
% :- mode firstTen( out, out ) is det.
firstTen( I, B ) :-
  B = (if list.member( I, [1,2,3,4,5,6,7,8,9,10] ) then yes else no).

:- pred firstTen( int ).
:- mode firstTen( in ) is semidet.
% :- mode firstTen( out ) is multi.
firstTen( I ) :- firstTen( I, yes ).

:- pred f( int::out ) is nondet.
f( I ) :-
  solutions( firstTen, FT )
  , list.member( I, FT )
  , I \= 3.

main(!IO) :-
  io.write_string( "hi\n", !IO ).
