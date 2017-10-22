%% @author Maciej Znamirowski

-module(various).

-export([area/1, volume/1, len/1, amin/1, amax/1,
		 tuple_min_max/1, list_min_max/1, areas/1,
		 dec_list/1, temp_conv/2, ones/1, repeatEl/2]).


%% geometric shapes areas

area({square,X,Y}) ->  X*Y;

area({circle,X}) -> X*X*math:pi();

area({triangle,A,H}) -> 0.5*A*H;

area({trapezoid,A,B,H}) -> 0.5*(A+B)*H;

area({sphere,R}) -> 4*R*math:pi();

area({cube,A}) -> 6*A*A;

area({cone,R,H}) -> (R + math:sqrt(R*R + H*H))*R*math:pi().


%% geometric shapes volumes

volume({sphere,R}) -> 4/3*math:pi()*math:pow(R, 3);

volume({cube,A}) -> math:pow(A, 3);

volume({cone,R,H}) -> 1/3*R*R*H*math:pi().


%% list length, minimum and maximum

len([]) -> 0;

len([_|T]) -> 1 + len(T).


amin([]) -> throw(empty_list_err());

amin([X]) -> X;

amin([X|T]) ->
	Y = amin(T),
	if
		X < Y -> X;
		true -> Y
	end.


amax([]) -> throw(empty_list_err());

amax([X]) -> X;

amax([X|T]) ->
	Y = amax(T),
	if
		X < Y -> Y;
		true -> X
	end.


tuple_min_max([]) -> throw(empty_list_err());

tuple_min_max(L) -> {amin(L), amax(L)}.

list_min_max([]) -> throw(empty_list_err());

list_min_max(L) -> [amin(L), amax(L)].


empty_list_err() -> {error,emptyList}.


%% areas of the geometric shapes from the list

areas([]) -> [];

areas([H|T]) -> [area(H) | areas(T)].


%% decreasing numbers list generator

dec_list(1) -> [1];

dec_list(N) when N > 0 -> [N | dec_list(N - 1)].


%% temperature converter

temp_conv({kelvin,T},kelvin) -> {kelvin, T};

temp_conv({kelvin,T},B) -> {B, convert({kelvin,T},B)};

temp_conv(A,kelvin) -> {kelvin, convert(A,kelvin)};

temp_conv(A,B) ->
	KelvinTemp = convert(A,kelvin),
	temp_conv({kelvin,KelvinTemp},B).

convert({kelvin,T},celsius) -> T - 273.15;

convert({kelvin,T},fahrenheit) -> 1.8*T - 459.67;

convert({kelvin,T},rankine) -> 1.8*T;

convert({kelvin,T},delisle) -> (373.15 - T)*1.5;

convert({kelvin,T},newton) -> (T - 273.15)*0.33;

convert({kelvin,T},romer) -> (T - 273.15)*0.525 + 7.5;

convert({kelvin,T},reaumur) -> (T - 273.15)*0.8;

convert({celsius,T},kelvin) -> 273.15 + T;

convert({fahrenheit,T},kelvin) -> (T + 459.67)*5/9;

convert({rankine,T},kelvin) -> T*5/9;

convert({delisle,T},kelvin) -> 373.15 - T*2/3;

convert({newton,T},kelvin) -> T*100/33 + 273.15;

convert({romer,T},kelvin) -> (T - 7.5)*40/21 + 273.15;

convert({reaumur,T},kelvin) -> T*1.25 + 273.15.


%% list of ones

ones(0) -> [];

ones(N) when N >= 0 -> [1 | ones(N - 1)].


%% list of N elements

repeatEl(0,_) -> [];

repeatEl(N,El) when N >= 0 -> [El | repeatEl(N - 1, El)].