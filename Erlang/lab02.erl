%% @author Maciej Znamirowski

-module(lab02).
-export([map_append/3, map_update/3, map_display_v1/1, map_display_v2/1, tokenize/1]).


%% updating and appending to map without 'maps' module

map_append(Key, Value, Map) -> Map#{Key => Value}.

map_update(Key, Value, Map) -> Map#{Key := Value}.


%% better map view

map_display_v1(Map) -> print_element(maps:keys(Map), Map).

print_element([Key|Keys], Map) ->
	io:format("~p: ~p~n", [Key, maps:get(Key,Map)]),
	print_element(Keys, Map);

print_element([], _) -> io:format("").


map_display_v2(Map) -> maps:fold(fun format_element/3, "", Map).

format_element(Key, Value, _) -> io:format("~p: ~p~n", [Key, Value]).


%% convert file to list of words with repetition number

tokenize(FileName) ->
	{ok, File} = file:read_file(FileName),
	Tokens = find_tokens(File, <<>>, #{}),
	file:close(File),
	Tokens.

find_tokens(<<>>, <<>>, Tokens) ->
	Tokens;

find_tokens(<<>>, Word, Tokens) ->
	add_or_inc(binary:bin_to_list(Word), Tokens);

find_tokens(<<Char, Rest/binary>>, Word, Tokens) ->
	next_char(Rest, Word, Tokens, Char, is_letter(Char)).

next_char(Rest, Word, Tokens, Char, true) ->
	find_tokens(Rest, <<Word/binary, Char>>, Tokens);

next_char(Rest, <<>>, Tokens, _, false) ->
	find_tokens(Rest, <<>>, Tokens);

next_char(Rest, Word, Tokens, _, false) ->
	find_tokens(Rest, <<>>, add_or_inc(binary:bin_to_list(Word), Tokens)).
	
	
is_letter(Char) when Char >= $a, Char =< $z -> true;
is_letter(Char) when Char >= $A, Char =< $Z -> true;
is_letter(_) -> false.

add_or_inc(Word, Tokens) ->
	case maps:find(Word, Tokens) of
		{ok,X} -> maps:put(Word, X + 1, Tokens);
		_ -> maps:put(Word, 1, Tokens)
	end.
