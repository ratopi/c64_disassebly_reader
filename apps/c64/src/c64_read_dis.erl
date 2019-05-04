%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%% Reading C64 Kernal and Basic Dissambly and outputs binary file
%%% @end
%%% Created : 04. Mai 2019 08:08
%%%-------------------------------------------------------------------
-module(c64_read_dis).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-record(state, {in, out, line = 0, command_lines = 0, write_count = 0, start_address, address = start}).

%% API
-export([convert/2]).
%-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

convert(InFile, OutFile) ->
	{ok, InIO} = file:open(InFile, [read, binary]),
	{ok, OutIO} = file:open(OutFile, [write]),
	State = #state{in = InIO, out = OutIO},
	io:fwrite("State ~p~n", [State]),
	read_line(State).


%%%===================================================================
%%% Internal functions
%%%===================================================================


read_line(State) ->
	case file:read_line(State#state.in) of

		{ok, Line} ->
			case handle_line(Line, State) of
				{ok, NewState, converted} ->
					read_line(NewState);
				{ok, NewState, ignored} ->
					read_line(NewState);
				{error, State, Reason} ->
					print_error(Reason, State)
			end;

		eof ->
			file:close(State#state.in),
			ok = file:close(State#state.out),
			io:fwrite("written ~p Bytes~n", [State#state.write_count]),
			io:fwrite("Final State~n~p~n", [State])
	end.


print_error(Reason, State) ->
	file:close(State#state.in),
	file:close(State#state.out),
	io:fwrite("ERROR~n~p~n~nState:~n~p~n", [Reason, State]).


handle_line(Line = <<$., Letter, Content/binary>>, State) when Letter == $,; Letter == $: ->
	ParseResult = hex:parse_multi(Content),
	case ParseResult of

		{ok, [{Address, 4} | ListOfBytes = [_ | _]], _Rest} ->
			InterimState =
				case State#state.address of
					start ->
						State#state{start_address = Address, address = Address};
					Address ->
						State;
					_ ->
						io:fwrite("ADDR CONFLICT ~p ~p~n", [Address, State#state.address]),
						FillBytes = create_null_bytes(Address - State#state.address),
						{ok, S} = write_bytes(FillBytes, State),
						S
				end,
			Bytes = convert_to_values(ListOfBytes),
			{ok, NewState} = write_bytes(Bytes, InterimState),
			{ok, NewState#state{line = State#state.line + 1, command_lines = State#state.command_lines + 1}, converted};

		{error, Reason} ->
			{error, State, Reason};

		_ ->
			{error, State, {strange_line, Line}}
	end;

handle_line(<<Letter, Bin/binary>>, State) when Letter =/= $. ->
	{ok, State#state{line = State#state.line + 1}, ignored}.




write_bytes(Bytes, State) ->
	ok = file:write(State#state.out, Bytes),
	L = length(Bytes),
	{
		ok,
		State#state{
			write_count = State#state.write_count + L,
			address = State#state.address + L
		}
	}.



convert_to_values(ListOfBytes) ->
	convert_to_values(ListOfBytes, []).



convert_to_values([], List) ->
	lists:reverse(List);

convert_to_values([{Value, 2} | T], List) ->
	convert_to_values(T, [Value | List]);

convert_to_values(_Values, List) -> % wrong count of digits =/= 2 .. could be a mnemonic, like ADC
	List.


create_null_bytes(Count) ->
	[0 || _ <- lists:seq(1, Count)].
