-module(zad01).
-export([create_tree/0, create_tree/1, insert/2]).

-record(node, {value = empty, left = empty, right = empty}).


create_tree() ->
	#node{}.
	
create_tree(Values) ->
	Tree = create_tree(),
	from_list(Values, Tree).

from_list([], Tree) ->
	Tree;

from_list([H|T], Tree) ->
	from_list(T, insert(H, Tree)).

insert(El, #node{value = empty}) ->
	#node{value=El};
	
insert(El, empty) ->
	#node{value=El};
	
insert(El, #node{value=V, left=L, right=R}) when El < V ->
	#node{value=V, left=insert(El, L), right=R};

insert(El, #node{value=V, left=L, right=R}) ->
	#node{value=V, left=L, right=insert(El, R)}.
	
