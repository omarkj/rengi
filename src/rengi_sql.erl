-module(rengi_sql).

-export([translate/2]).

-define(COMMA, <<",">>).
-define(OPEN_PAR, <<"(">>).
-define(CLOSE_PAR, <<")">>).

translate(Dsl, Opts) ->
    binary:list_to_bin(lists:flatten(translate(Dsl, Opts, []))).

translate([], _Opts, Retval) ->
    Retval;
translate([{insert, Table, {columns, Columns,
			    {values, Values}}}|Rest], Opts, Retval) ->
    Columns0 = handle_columns(Columns),
    Values0 = handle_values(Values),
    translate(Rest, Opts, Retval++[<<"INSERT INTO ">>, Table, <<" ">>, 
				   Columns0, <<" VALUES ">>,
				   Values0, <<";">>]).

handle_columns(Columns) ->
    parenthesis(comma(Columns)).

handle_values(Values) ->
    handle_values(Values, []).

handle_values([Value|[]], Retval) ->
    Retval++parenthesis(comma(handle_value(Value, [])));
handle_values([Value|Values], Retval) ->
    handle_values(Values, Retval++trailing_comma(parenthesis(comma(handle_value(Value, []))))).

handle_value([], Retval) ->
    Retval;
handle_value([Value|Values], Retval) ->
    handle_value(Values, Retval++[to_bin(Value)]).

parenthesis(Values) ->
    [?OPEN_PAR, Values, ?CLOSE_PAR].

trailing_comma(Val) ->
    Val++[?COMMA].

comma(Values) ->
    comma(Values, []).
    
comma([], Retval) ->
    Retval;
comma([Value|Values], []) ->
    comma(Values, [Value]);
comma([Value|Values], Retval) ->
    comma(Values, Retval++[?COMMA, Value]).


to_bin(Bin) ->
    to_bin(Bin, true).

to_bin(Bin, true) when is_binary(Bin) ->
    <<"'", Bin/binary, "'">>;
to_bin(Bin, _) when is_binary(Bin) ->
    Bin;
to_bin(List, _Escape) when is_list(List) ->
    to_bin(list_to_binary(List), true);
to_bin(Integer, _Escape) when is_integer(Integer) ->
    to_bin(hd(io_lib:fwrite("~p", [Integer])), false);
to_bin(Float, _Escape) when is_float(Float) ->
    to_bin(hd(io_lib:fwrite("~p", [Float])), false).
