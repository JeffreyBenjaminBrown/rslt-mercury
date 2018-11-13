:- module rslt.
:- interface.
:- import_module io, int, string, list, hash_table, set.
:- pred main(io::di, io::uo) is det.

% === types ===
:- type address == int. % Each expr has one of these.

:- type arity == int.

:- type expr ---> word( string )
                ; template( list( address ) ) % its component words
  % For instance, in the relationship template "Every _ requires _ unless _"
  % the component words are "The", "requires", "unless" and "".
                ; rel( address            % its template
                     , list( address ) ). % its members
  % In the relationship "#Every human #requires water #unless magic",
  % the template is the one described earlier, and the members are
  % "human", "water" and "magic".

:- type role ---> roleTemplate ; roleMember( int ).
  % If it's involved in a rel, it's either the template or one of the members.

:- type position ---> (address, role).
  % Like a position at an employer, these include both the host
  % and the role played there. The address should belong to a Rel.

:- type exprImg ---> % An image (diagram, mugshot) of an expr --
                     % something that lets you find it.
    imgAddress
    ; imgWord( string )
    ; imgTemplate( list( exprImg ) )
    ; imgRel( exprImg, list( exprImg ) ).

:- type graph ---> graph(
    hash_table( address, expr )          % "what is there?"
  , hash_table( exprImg, address )       % "where is it?"
  , hash_table( address, set(position) ) % "what roles does it play where?"
  , hash_table( position, address ) ).   % "what is in this position?"


% === dumb tests ===
:- implementation.
:- func id(hash_table(K,V)) = hash_table(K,V).
id( HT ) = HT.

main(IOState_in, IOState_out) :-
  io.write_string("Hello, World!\n", IOState_in, IOState_out).
