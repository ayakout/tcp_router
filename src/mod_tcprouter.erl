%% @todo Change Backend and route to records {ip, port, lastused} and {id, port, proc}
-module(mod_tcprouter).
-export([do/1]).
-include_lib("inets/src/http_server/httpd.hrl").
-define(MAX_PORT, 65535).
-include("tcp_router.hrl").

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


%% @doc Proceed with the response
proceed(Info, Data) ->
    io:format("~s: ~p ~n", [?MODULE, Info#mod.request_uri]),
    Head=[{content_type, "text/xml"},
          {content_length, integer_to_list(httpd_util:flatlength(Data))},
          {code, 200}],
    {proceed, [{response,{response, Head, Data}} | Info#mod.data]}.


handle_post(Info) ->
    Params = parse_query(Info#mod.request_uri),
    Body = parse_body(Info#mod.entity_body),
    App = list_to_integer(proplists:get_value(apps, Params)),
    RouteId = list_to_integer(proplists:get_value(routes, Params)),
    BackendId = list_to_integer(proplists:get_value(backends, Params)),
    [AppRoute] = mnesia:dirty_match_object({app_route, App, RouteId, '_', '_'}),
    [RouteBackend] = mnesia:dirty_match_object({route_backend, AppRoute#app_route.route, 
                                                BackendId, '_', '_', '_'}),
    %% @todo transaction
    mnesia:dirty_delete_object(route_backend, RouteBackend),
    mnesia:dirty_write(RouteBackend#route_backend{ip = proplists:get_value(ip, Body),
                                                  port = proplists:get_value(port, Body),
                                                  lastused = 0}).


handle_delete(["routes", Id], Info) ->
    Params = parse_query(Info#mod.request_uri),
    delete_route(Id, Params);
handle_delete(["backends", Id], Info) ->
    Params = parse_query(Info#mod.request_uri),
    delete_backend(Id, Params);
handle_delete(_, _) ->
    [].
    

%% @doc Process 'GET' actions
handle_get("routes", Info) ->
    Params = parse_query(Info#mod.request_uri),
    App = list_to_integer(proplists:get_value(apps, Params)),
    AppRoutes = mnesia:dirty_read(app_route, App),
    F = fun(#app_route{id = Id, route = Route}) ->
                RouteBackends = mnesia:dirty_read(route_backend, Route),
                [lists:concat([" -> {id: ", Id, ", url: tcp://localhost:", Route, 
                              ", backends: [{ip: ", B#route_backend.ip, ", port: ", B#route_backend.port, "}]}\n"])
                 || B <- RouteBackends]
        end,
    lists:map(F, AppRoutes);
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
    {ok, Pid} = tcp_proc_sup:start_proc(Port),
    %% @todo transaction
    PortId = case mnesia:dirty_read(app_route, App) of
                 [] -> 1;
                 AppRoutes ->
                     LastAppRoute = lists:last(lists:keysort(#app_route.id, AppRoutes)),
                     LastAppRoute#app_route.id + 1
             end,
    mnesia:dirty_write(#app_route{app = App, id = PortId, route = Port, proc = Pid}),
    lists:concat([" -> {Id: ", PortId, ", url: tcp://localhost:", Port, "}\n"]). 


delete_route(Id, Params) ->
    App = list_to_integer(proplists:get_value(apps, Params)),
    IdInt = list_to_integer(Id),
    [AppRoute] = mnesia:dirty_match_object({app_route, App, IdInt, '_', '_'}),
    mnesia:dirty_delete_object(app_route, AppRoute),
    true = exit(AppRoute#app_route.proc, shutdown), %% @todo Add stop API in tcp_proc
    [{free_ports, FreePorts}] = ets:lookup(tcp_app_routes, free_ports),
    ets:insert(tcp_app_routes, {free_ports, [AppRoute#app_route.route | FreePorts]}).
    

%% @doc Add a backend to the application route
add_backend(Params, Body) ->
    App = list_to_integer(proplists:get_value(apps, Params)),
    RouteId = list_to_integer(proplists:get_value(routes, Params)),
    case mnesia:dirty_match_object({app_route, App, RouteId, '_', '_'}) of
        [] -> [];
        [AppRoute] ->
            #app_route{route = Route} = AppRoute,
            BackendId = case mnesia:dirty_read(route_backend, Route) of
                            [] -> 
                                1;
                            RouteBackends ->
                                LastRouteBackend = lists:last(lists:keysort(#route_backend.id, RouteBackends)),
                                LastRouteBackend#route_backend.id + 1
                        end,
            io:format("Adding Backend~p for route ~p port: ~p~n", [Body, RouteId, Route]),
            mnesia:dirty_write(#route_backend{route = Route, id = BackendId, 
                                              ip = proplists:get_value(ip, Body),
                                              port = proplists:get_value(port, Body)}),
            lists:concat([" -> {Id: ", RouteId, "}\n"])
    end.


delete_backend(Id, Params) ->
    App = list_to_integer(proplists:get_value(apps, Params)),
    RouteId = list_to_integer(proplists:get_value(routes, Params)),
    [AppRoute] = mnesia:dirty_match_object({app_route, App, RouteId, '_', '_'}),
    [RouteBackend] = mnesia:dirty_match_object({route_backend, AppRoute#app_route.route, 
                                                list_to_integer(Id), '_', '_', '_'}),
    mnesia:dirty_delete_object(route_backend, RouteBackend).
        
    
%% @doc Retrieve the next available port in sequence and increments the index
next_port() ->
    [{free_ports, FreePorts}] = ets:lookup(tcp_app_routes, free_ports),
    case FreePorts of
        [] ->
            Port = ets:update_counter(tcp_app_routes, next_port, 1),
            if 
                Port > ?MAX_PORT -> 
                    throw(no_ports_available);
                true -> 
                    Port
            end;
        [Port | Rest] ->
            ets:insert(tcp_app_routes, {free_ports, Rest}),
            Port
    end.
    

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
                   
               
