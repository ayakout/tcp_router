-module(tcp_conf).
-export([load_balance/0]).
-define(APP, tcp_router).


load_balance() ->
    get_env(load_balance).


get_env(Key) ->
    case application:get_env(tcp_router, load_balance) of
        {ok, Value} ->
            Value;
        undefined ->
            throw({undefined_config, Key})
    end.
