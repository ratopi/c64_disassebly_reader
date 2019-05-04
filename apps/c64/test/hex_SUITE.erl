%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 04. Mai 2019 12:04
%%%-------------------------------------------------------------------
-module(hex_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").


%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([test_positive/1, test_negativ/1, test_multi_positiv/1]).

all() -> [
	test_positive,
	test_negativ,
	test_multi_positiv
].

% ---

init_per_suite(Config) ->
	Config.


end_per_suite(_Config) ->
	ok.

init_per_testcase(_, Config) ->
	Config.


end_per_testcase(_, _Config) ->
	ok.

% ---

test_positive(_Config) ->
	{ok, {16#80, 2, <<>>}} = hex:parse(<<"80">>),
	{ok, {16#123, 3, <<>>}} = hex:parse(<<"123">>),
	{ok, {16#E312, 4, <<" ">>}} = hex:parse(<<"E312 ">>).

test_negativ(_Config) ->
	{error, {illegal_letter, $R}} = hex:parse(<<"BRK">>),
	{error, {illegal_letter, 32}} = hex:parse(<<" 80">>).



test_multi_positiv(_Config) ->
	{
		ok,
		[
			{16#E123, 4},
			{16#AF, 2},
			{16#31, 2},
			{16#23, 2}
		],
		<<"BRK">>
	} = hex:parse_multi(<<"E123 AF 31 23 BRK">>),

	{
		ok,
		[
			{16#E123, 4},
			{16#AF, 2},
			{16#31, 2},
			{16#23, 2}
		],
		<<"  80">>
	} = hex:parse_multi(<<"E123 AF 31 23   80">>).
