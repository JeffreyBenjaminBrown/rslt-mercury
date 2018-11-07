:- module rslt.
:- interface.
:- import_module io, int, string, list.
:- pred main(io::di, io::uo) is det.
:- type index == int.
:- type arity == int.
:- type position == int.
:- type expr ---> word( string )
                ; tplt( list( string ) )
                ; rel.
:- implementation.
:- import_module hash_table.

:- func id(hash_table(K,V)) = hash_table(K,V).
id( HT ) = HT.

main(IOState_in, IOState_out) :-
  io.write_string("Hello, World!\n", IOState_in, IOState_out).
