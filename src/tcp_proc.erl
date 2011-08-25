%% @todo Refactor tunnel_connection proc to a gen_server
-module(tcp_proc).
-export([start_link/1]).
-export([init/2]).
-export([tunnel_connection/3]).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).
-define(TIMEOUT, 60000).
-include("tcp_router.hrl").


start_link(Port) ->
    proc_lib:start_link(?MODULE, init, [self(), Port]).


init(Parent, Port) ->
    case gen_tcp:listen(Port, [{reuseaddr, true} | ?TCP_OPTIONS]) of
        {ok, Socket} -> 
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(Port, Socket);
        {error, Reason} ->
            exit(Reason)
    end.


loop(Port, Socket) ->
    loop(Port, Socket, 1).
loop(Port, Socket, Nth) ->
    case gen_tcp:accept(Socket) of
        {ok, Client} ->
            Backends = mnesia:dirty_read(route_backend, Port),
            io:format("Connecting client ~p to backends ~p~n", [Client, Backends]),
            if 
                Backends == [] ->
                    gen_tcp:close(Client),
                    loop(Port, Socket, 1);
                true -> 
                    {Backend, NextNth} = load_balance(Backends, Nth, tcp_conf:load_balance()),
                    {value, _, BackendsRest} = lists:keytake(Backend#route_backend.id, #route_backend.id, Backends),
                    io:format("Backend selected: ~p~n", [Backend]),
                    Pid = proc_lib:spawn(?MODULE, tunnel_connection, [Client, Backend, BackendsRest]),
                    gen_tcp:controlling_process(Client, Pid),
                    loop(Port, Socket, NextNth)
            end;
        {error, Reason} ->
            error_logger:info_msg("Failed to accept connection ~p", [Reason]),
            loop(Port, Socket, Nth)
    end.
            

tunnel_connection(Client, Backend, BackendsRest) ->
    erlang:yield(),
    io:format("Connecting to backend ~p...~n", [Backend]),
    case gen_tcp:connect(Backend#route_backend.ip, list_to_integer(Backend#route_backend.port), 
                         ?TCP_OPTIONS) of
        {ok, BackendSock} ->
            try 
                send_receive(Client, BackendSock)
            catch 
                _:_ -> 
                    gen_tcp:close(Client)
            after 
                gen_tcp:close(BackendSock),
                update_lastused(Backend)
            end;
        {error, _Reason} when BackendsRest == [] ->
            gen_tcp:close(Client);
        {error, _Reason} ->
            %% Retry with the next backend in list order
            [H | Rest] = BackendsRest,
            tunnel_connection(Client, H, Rest)
    end.


send_receive(Client, BackendSock) ->
    inet:setopts(Client, [{active, once}]),
    inet:setopts(BackendSock, [{active, once}]),
    receive
        {tcp, Client, ClientData} ->
            ok = gen_tcp:send(BackendSock, ClientData),
            send_receive(Client, BackendSock);
        {tcp, BackendSock, BackendData} ->
            ok = gen_tcp:send(Client, BackendData),
            send_receive(Client, BackendSock);
        {tcp_closed, _} -> 
            ok;
        {tcp_error, _ , _} ->
            ok
    end.


load_balance(Backends, Nth, rr) 
  when Nth == length(Backends) ->
    {lists:nth(Nth, Backends), 1};
load_balance(Backends, Nth, rr) ->
    {lists:nth(Nth, Backends), Nth + 1};
load_balance(Backends, Nth, lru) -> 
    {hd(lists:keysort(#route_backend.lastused, Backends)), Nth}.


update_lastused(Backend) ->
    %% @todo transaction
    mnesia:dirty_delete_object(route_backend, Backend),
    mnesia:dirty_write(Backend#route_backend{lastused = now()}).
