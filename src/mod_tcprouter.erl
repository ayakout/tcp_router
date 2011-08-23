-module(mod_tcprouter).
-export([do/1]).
-include_lib("inets/src/http_server/httpd.hrl").


%% @doc Inets handler function
do(#mod{method = "PUT"} = Info) ->
    case proplists:get_value(status, Info#mod.data) of
        {_StatusCode, _PhraseArgs, _Reason} ->
            {proceed, Info#mod.data};
        undefined ->
            case proplists:get_value(response, Info#mod.data) of
                undefined ->
                    Action = lists:last(string:tokens(Info#mod.request_uri, "/")),
                    Data = handle_put(Action, Info),
                    proceed(Info, Data);
                _Response ->
                    {proceed, Info#mod.data}
            end
    end;
do(#mod{method = "POST"} = Info) ->
    case proplists:get_value(status, Info#mod.data) of
        {_StatusCode, _PhraseArgs, _Reason} ->
            {proceed, Info#mod.data};
        undefined ->
            case proplists:get_value(response, Info#mod.data) of
                undefined ->
                    handle_post(Info),
                    proceed(Info, "");
                _Response ->
                    {proceed, Info#mod.data}
            end
    end;
do(#mod{method = "GET"} = Info) ->
    case proplists:get_value(status, Info#mod.data) of
        {_StatusCode, _PhraseArgs, _Reason} ->
            {proceed, Info#mod.data};
        undefined ->
            case proplists:get_value(response, Info#mod.data) of
                undefined ->
                    Action = lists:last(string:tokens(Info#mod.request_uri, "/")),
                    Data = handle_get(Action, Info),
                    proceed(Info, Data);
                _Response ->
                    {proceed, Info#mod.data}
            end
    end;
do(#mod{method = "DELETE"} = Info) ->
    case proplists:get_value(status, Info#mod.data) of
        {_StatusCode, _PhraseArgs, _Reason} ->
            {proceed, Info#mod.data};
        undefined ->
            case proplists:get_value(response, Info#mod.data) of
                undefined ->
                    %% Last 2 params in the request_uri
                    Action = string:right(string:tokens(Info#mod.request_uri, "/"), 2),
                    handle_delete(Action, Info),
                    proceed(Info, "");
                _Response ->
                    {proceed, Info#mod.data}
            end
    end;
do(Info) ->
    Info.


%% @doc Returns dummy response
proceed(Info, Data) ->
    io:format("~s: ~p ~n", [?MODULE, Info#mod.request_uri]),
    Head=[{content_type, "text/xml"},
          {content_length, integer_to_list(httpd_util:flatlength(Data))},
          {code, 401}],
    {proceed, [{response,{response, Head, Data}} | Info#mod.data]}.


handle_post(Info) ->
    Params = parse_query(Info#mod.request_uri),
    Body = parse_body(Info#mod.entity_body),
    App = list_to_integer(proplists:get_value(apps, Params)),
    Route = list_to_integer(proplists:get_value(routes, Params)),
    BackendId = list_to_integer(proplists:get_value(backends, Params)),
    [{App, Ports}] = ets:lookup(tcp_app_routes, App),
    Port = proplists:get_value(Route, Ports),
    [{Port, Backends}] = ets:lookup(tcp_route_backends, Port),
    ets:insert(tcp_route_backends, {Port, lists:keyreplace(BackendId, 1, Backends, {BackendId, Body})}).


handle_delete(["routes", Id], Info) ->
    Params = parse_query(Info#mod.request_uri),
    App = list_to_integer(proplists:get_value(apps, Params)),
    [{App, Ports}] = ets:lookup(tcp_app_routes, App),
    ets:insert(tcp_app_routes, {App, proplists:delete(list_to_integer(Id), Ports)});
handle_delete(["backends", Id], Info) ->
    Params = parse_query(Info#mod.request_uri),
    App = list_to_integer(proplists:get_value(apps, Params)),
    Route = list_to_integer(proplists:get_value(routes, Params)),
    [{App, Ports}] = ets:lookup(tcp_app_routes, App),
    Port = proplists:get_value(Route, Ports),
    [{Port, Backends}] = ets:lookup(tcp_route_backends, Port),
    ets:insert(tcp_route_backends, {Port, proplists:delete(list_to_integer(Id), Backends)});
handle_delete(_, _) ->
    [].
    

%% @doc Process 'GET' actions
handle_get("routes", Info) ->
    Params = parse_query(Info#mod.request_uri),
    App = list_to_integer(proplists:get_value(apps, Params)),
    [{App, Ports}] = ets:lookup(tcp_app_routes, App),
    F = fun({Id, Port}) ->
                [{Port, Backends}] = ets:lookup(tcp_route_backends, Port),
                [lists:concat([" -> {id: ", Id, ", url: tcp://localhost:", Port, 
                              ", backends: ", lists:flatten(io_lib:format("~p", [Backend])), "}\n"])
                 || {_, Backend} <- Backends]
        end,
    lists:map(F, Ports);
handle_get(_, _) ->
    [].

%% @doc Process 'PUT' actions
handle_put("routes", Info) ->
    Params = parse_query(Info#mod.request_uri),
    io:format("Params: ~p~n", [Params]),
    create_route(Params);
handle_put("backends", Info) ->
    Params = parse_query(Info#mod.request_uri),
    Body = parse_body(Info#mod.entity_body),
    io:format("Params: ~p Body:~p~n", [Params, parse_body(Info#mod.entity_body)]),
    add_backend(Params, Body);
handle_put(_, _) ->
    "Unhandled request".


%% @doc Populates the routes table and backends and starts a TCP Port server
create_route(Params) ->
    App = list_to_integer(proplists:get_value(apps, Params)),
    Port = next_port(),
    io:format("Creating new route ~p for ~p~n", [Port, App]),    
    PortId = case ets:lookup(tcp_app_routes, App) of
                 [] -> 
                     ets:insert(tcp_app_routes, {App, [{1, Port}]}),
                     1;
                 [{App, [{Id, _} | _] = Ports}] ->
                     ets:insert(tcp_app_routes, {App, [{Id + 1, Port} | Ports]}),
                     Id + 1
             end,
    ets:insert(tcp_route_backends, {Port, []}),
    tcp_proc:start_link(Port),
    lists:concat([" -> {Id: ", PortId, ", url: tcp://localhost:", Port, "}\n"]). 


%% @doc Add a backend to the application route
add_backend(Params, Body) ->
    App = list_to_integer(proplists:get_value(apps, Params)),
    case ets:lookup(tcp_app_routes, App) of
        [] -> "Not Found";
        [{_App, Routes}] ->
            Route = list_to_integer(proplists:get_value(routes, Params)),
            Port = proplists:get_value(Route, Routes),
            io:format("Adding Backend ~p for route ~p port: ~p~n", [Body, Route, Port]),
            case ets:lookup(tcp_route_backends, Port) of
                [] -> ok;
                [{Port, Backends}] when Backends == [] ->
                    ets:insert(tcp_route_backends, {Port, [{1, Body}]});
                [{Port, [{Id, _} | _] = Backends}] ->
                    ets:insert(tcp_route_backends, {Port, [{Id + 1, Body} | Backends]})
            end,
            lists:concat([" -> {Id: ", Route, "}\n"])
    end.


%% @doc Retrieve the next available port in sequence and increments the index
next_port() ->
    ets:update_counter(tcp_app_routes, next_port, 1).
    

%% @doc Parse the URI into a tagged list        
parse_query(String) ->
    Params = string:tokens(String, "/"),
    params(Params).

params(Params) ->
    params(Params, []).
params([], Acc) ->
    Acc;
params([Key, Value | Rest], Acc) ->
    params(Rest, [{list_to_atom(Key), Value} | Acc]);
params([Key | Rest], Acc) ->
    params(Rest, [{list_to_atom(Key)} | Acc]).


%% Awkward body parsing..
parse_body(String) ->
    Tokens = string:tokens(String, "{,}"),
    F = fun(X) -> [Key, Value] = string:tokens(X, ": "), 
                  {list_to_atom(string:strip(Key, both, $")), string:strip(Value, both, $")} end,
    lists:map(F, Tokens).
                   
               
