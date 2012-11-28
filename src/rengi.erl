-module(rengi).

-export([process/1,
	 process/2]).

-include("rengi.hrl").

process(Dsl) ->
    process(Dsl, [{translator, rengi_sql}]).

process(Dsl, Opts) ->
    Translator = proplists:get_value(translator, Opts, rengi_sql),
    Translator:translate(tokenize(Dsl), Opts).

tokenize({insert, TableName, {values, Values}}) ->
    handle_insert(TableName, Values);
tokenize({insert, TableName, Values}) ->
    handle_insert(TableName, Values).

handle_insert(TableName, Values) when is_list(hd(Values)) ->
    Columns = proplists:get_keys(hd(Values)),
    ValuesList = handle_values(Values, Columns),
    [{insert, TableName, {columns, Columns, {values, ValuesList}}}];
handle_insert(TableName, Values) ->
    handle_insert(TableName, [Values]).

handle_values(Values, Columns) ->
    handle_values(Values, Columns, []).

handle_values([], _, Retval) ->
    Retval;
handle_values([Value|Values], Columns, Retval) ->
    handle_values(Values, Columns, Retval++handle_value(Value, Columns, [])).

handle_value(ValueList, ColumnList, _) when length(ValueList) =/= length(ColumnList) ->
    throw(column_and_values_inequal);
handle_value([], [], Retval) ->
    [Retval];
handle_value(ValueList, [Column|Columns], Retval) ->
    ColumnValue = proplists:get_value(Column, ValueList),
    ValueList0 = proplists:delete(Column, ValueList),
    handle_value(ValueList0, Columns, Retval++[ColumnValue]).
