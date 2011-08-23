-module(tcp_router).
-export([start/0]).
-define(INITIAL_ROUTE, 10000).
-define(ROUTER_PORT, 8001).


start() ->
    ets:new(tcp_app_routes, [public, named_table]),
    ets:new(tcp_route_backends, [public, named_table]),
    ets:insert(tcp_app_routes, {next_port, ?INITIAL_ROUTE - 1}),
    inets:start(httpd, instance(?MODULE_STRING, ?ROUTER_PORT, [{all}])).


instance(Name, Port, Handlers) ->
    [{server_name, Name},
     {server_root, "."},
     {document_root, "."},
     {port, Port},
     {modules, [mod_tcprouter]},
     {mime_types, [{".xml", "text/xml"}]},
     {handlers, Handlers}].
