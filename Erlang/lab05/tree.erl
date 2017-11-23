-module(tree).
-export([createTree/0, listToTree/1, insert/2, printTree/1, randomTree/2,
		 treeToList/1, treeToList/2, find/2, find2/2]).
-import(rand, [uniform/1]).
-import(lists, [foldl/3, append/1]).

-record(node, {v = empty, l = empty, r = empty}).


createTree() -> #node{}.

listToTree(List) -> foldl(fun(E, T) -> insert(E, T) end, createTree(), List).

insert(El, #node{v = empty}) -> #node{v = El};
insert(El, empty) -> #node{v = El};
insert(El, N = #node{v = V, l = L}) when El < V -> N#node{l = insert(El, L)};
insert(El, N = #node{r = R}) -> N#node{r = insert(El, R)}.

printTree(#node{v = empty}) -> empty;
printTree(Tree) -> print(Tree, 0).

print(empty, Level) ->
	printIndent(Level),
	io:format(".~n");

print(#node{v = V, l = L, r = R}, Level) ->
	printIndent(Level),
	io:format("~p~n", [V]),
	print(L, Level + 1),
	print(R, Level + 1).

printIndent(0) ->
	io:format("");

printIndent(N) ->
	io:format("  "),
	printIndent(N-1).


randomTree(N, Range) ->
	generateRandoms(N, Range, createTree()).

generateRandoms(0, _Range, Tree) ->
	Tree;

generateRandoms(N, Range, Tree) ->
	insert(uniform(Range), generateRandoms(N - 1, Range, Tree)).


treeToList(Tree) -> treeToList(Tree, inorder).

treeToList(empty, _) -> [];
treeToList(#node{v = V, l = L, r = R}, Type) ->
	Left = treeToList(L, Type),
	Right = treeToList(R, Type),
	Value = [V],
	case Type of
		preorder -> append([Value, Left, Right]);
		inorder -> append([Left, Value, Right]);
		postorder -> append([Left, Right, Value])
	end.


find(El, #node{v = El}) -> true;
find(_El, empty) -> false;
find(El, #node{v = V, l = L}) when El < V -> find(El, L);
find(El, #node{r = R}) -> find(El, R).


find2(El, Tree) ->
	try findExc(El, Tree) of
		false -> false
	catch
		true -> true
	end.

findExc(El, #node{v = El}) -> throw(true);
findExc(_El, empty) -> false;
findExc(El, #node{v = V, l = L}) when El < V -> findExc(El, L);
findExc(El, #node{r = R}) -> findExc(El, R).