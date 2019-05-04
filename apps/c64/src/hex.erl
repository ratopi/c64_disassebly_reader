%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 04. Mai 2019 11:13
%%%-------------------------------------------------------------------
-module(hex).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-record(hex, {whitespaces = [32, 13, 10, 8]}).

%% API
-export([parse_multi/1, parse/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(
parse_multi(Bin :: binary()) ->
	[{ok, N :: integer(), DigitCount :: integer(), Rest :: binary()}]
).
parse_multi(Bin) ->
	{ok, List, Rest} = parse_multi(Bin, [], start),
	{ok, lists:reverse(List), Rest}.


-spec(
parse(Bin :: binary()) ->
	{ok, N :: integer(), DigitCount :: integer(), Rest :: binary()}
	| {error, Reason :: term()}
).
parse(<<"">>) ->
	{error, empty_string};

parse(Bin) when is_binary(Bin) ->
	parse(Bin, 0, 0).


%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_multi(<<>>, List, _) ->
	{ok, List, <<>>};


parse_multi(Bin = <<Letter, Tail/binary>>, List, whitespace) ->
	case is_whitespace(Letter) of
		true ->
			parse_multi(Tail, List, start);
		false ->
			{ok, List, Bin}
	end;


parse_multi(Bin = <<Letter, Tail/binary>>, List, start) ->
	case parse(Bin) of
		E = {error, _Reason} ->
			case List of
				[] -> E;
				_ -> {ok, List, Bin}
			end;
		{ok, {N, D, Rest}} ->
			parse_multi(Rest, [{N, D} | List], whitespace)
	end.

% ---

parse(<<>>, N, DigitCount) ->
	{ok, {N, DigitCount, <<>>}};

parse(Bin = <<Letter, Rest/binary>>, N, DigitCount) ->
	case is_hex_digit(Letter) of

		false ->
			case is_whitespace(Letter) of
				true ->
					case DigitCount of
						0 ->
							{error, {illegal_letter, Letter}};
						_ ->
							{ok, {N, DigitCount, Bin}}
					end;
				false ->
					{error, {illegal_letter, Letter}}
			end;

		true ->
			NewN = convert_hex_digit(Letter) + N * 16,
			parse(Rest, NewN, DigitCount + 1)

	end.

% ---

convert_hex_digit($0) -> 0;
convert_hex_digit($1) -> 1;
convert_hex_digit($2) -> 2;
convert_hex_digit($3) -> 3;
convert_hex_digit($4) -> 4;
convert_hex_digit($5) -> 5;
convert_hex_digit($6) -> 6;
convert_hex_digit($7) -> 7;
convert_hex_digit($8) -> 8;
convert_hex_digit($9) -> 9;
convert_hex_digit($A) -> 10;
convert_hex_digit($B) -> 11;
convert_hex_digit($C) -> 12;
convert_hex_digit($D) -> 13;
convert_hex_digit($E) -> 14;
convert_hex_digit($F) -> 15;
convert_hex_digit($a) -> 10;
convert_hex_digit($b) -> 11;
convert_hex_digit($c) -> 12;
convert_hex_digit($d) -> 13;
convert_hex_digit($e) -> 14;
convert_hex_digit($f) -> 15.


is_hex_digit($0) -> true;
is_hex_digit($1) -> true;
is_hex_digit($2) -> true;
is_hex_digit($3) -> true;
is_hex_digit($4) -> true;
is_hex_digit($5) -> true;
is_hex_digit($6) -> true;
is_hex_digit($7) -> true;
is_hex_digit($8) -> true;
is_hex_digit($9) -> true;
is_hex_digit($A) -> true;
is_hex_digit($B) -> true;
is_hex_digit($C) -> true;
is_hex_digit($D) -> true;
is_hex_digit($E) -> true;
is_hex_digit($F) -> true;
is_hex_digit($a) -> true;
is_hex_digit($b) -> true;
is_hex_digit($c) -> true;
is_hex_digit($d) -> true;
is_hex_digit($e) -> true;
is_hex_digit($f) -> true;
is_hex_digit(_) -> false.


is_whitespace(32) -> true;
is_whitespace(13) -> true;
is_whitespace(10) -> true;
is_whitespace(9) -> true;
is_whitespace(_) -> false.
