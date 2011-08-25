-module(tcp_router).
-export([start/2, stop/1]).
-define(INITIAL_ROUTE, 10000).
-define(ROUTER_PORT, 8001).
-behaviour(application).
-include("tcp_router.hrl").

%% Mnesia install
-export([install/1]).

start(normal, []) ->
    ets:new(tcp_app_routes, [public, named_table]),
%    ets:new(tcp_route_backends, [public, named_table]),
    ets:insert(tcp_app_routes, {next_port, ?INITIAL_ROUTE - 1}),
    ets:insert(tcp_app_routes, {free_ports, []}),
    inets:start(httpd, instance(?MODULE_STRING, ?ROUTER_PORT, [{all}])),
    tcp_proc_sup:start_link().


stop([]) ->
    ets:delete(tcp_app_routes),
    ets:delete(tcp_route_backends),
    ok.
    

instance(Name, Port, Handlers) ->
    [{server_name, Name},
     {server_root, "."},
     {document_root, "."},
     {port, Port},
     {modules, [mod_tcprouter]},
     {mime_types, [{".xml", "text/xml"}]},
     {handlers, Handlers}].


%% @doc Install mnesia tables
install(Nodes) ->
    {atomic, ok} = mnesia:create_table(app_route, [{attributes, record_info(fields, app_route)}, 
                                                    {ram_copies, Nodes}, {type, bag}]),
    {atomic, ok} = mnesia:create_table(route_backend, [{attributes, record_info(fields, route_backend)},
                                                        {ram_copies, Nodes}, {type, bag}]),
    ok.


