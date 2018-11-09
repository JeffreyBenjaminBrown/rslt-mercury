:- module rslt.
:- interface.
:- import_module io, int, string, list.
:- pred main(io::di, io::uo) is det.
:- type index == int. % each expr has one of these, like an address
:- type arity == int.
:- type expr ---> word( string ) ; tplt ; rel.
:- type role ---> roleTemplate ; roleMember( int ).
:- type position ---> (index, role). % Like a position at an employer --
                                     % includes host and role played there.
                                     % Index should be of a Rel.
:- type exprImg ---> % An image (diagram, mugshot) of an expr --
                     % something that lets you find it.
    imgIndex
    ; imgWord( string )
    ; imgTemplate( list( exprImg ) )
    ; imgRel( exprImg, list( exprImg ) ).
:- implementation.
:- import_module hash_table.

:- func id(hash_table(K,V)) = hash_table(K,V).
id( HT ) = HT.

main(IOState_in, IOState_out) :-
  io.write_string("Hello, World!\n", IOState_in, IOState_out).
