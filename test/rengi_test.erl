-module(rengi_test).
-include_lib("eunit/include/eunit.hrl").

rengi_test_() ->
    {setup,
     fun() ->
	     ok
     end,
     fun(_Pid) ->
	     ok
     end,
     [
      {"INSERT test", ?_test(t_insert())}
     ]
    }.

t_insert() ->
    Insert = {insert, <<"trades">>,
	      {values, [[{<<"id">>, 10},
			 {<<"first_name">>, <<"John">>},
			 {<<"last_name">>, <<"Doe">>}],
			[{<<"id">>, 12},
			 {<<"first_name">>, <<"John">>},
			 {<<"last_name">>, <<"Doe">>}]]}},
    R = rengi:process(Insert, [{translate, rengi_sql}]),
    error_logger:info_msg("R is ~p", [R]).
