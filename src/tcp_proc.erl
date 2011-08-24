-module(tcp_proc).
-export([start_link/1]).
-export([init/2]).
-export([tunnel_connection/4]).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).
-define(TIMEOUT, 60000).


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
            [{Port, Backends}] = ets:lookup(tcp_route_backends, Port),
            io:format("Connecting client ~p to backends ~p~n", [Client, Backends]),
            if 
                Backends == [] ->
                    gen_tcp:close(Client),
                    loop(Port, Socket, 1);
                true -> 
                    {{Id,_,_} = Backend, NextNth} = load_balance(Backends, Nth, tcp_conf:load_balance()),
                    {value, _, BackendsRest} = lists:keytake(Id, 1, Backends),
                    io:format("Backend selected: ~p~n", [Backend]),
                    Pid = proc_lib:spawn(?MODULE, tunnel_connection, [Port, Client, Backend, BackendsRest]),
                    gen_tcp:controlling_process(Client, Pid),
                    loop(Port, Socket, NextNth)
            end;
        {error, Reason} ->
            error_logger:info_msg("Failed to accept connection ~p", [Reason]),
            loop(Port, Socket, Nth)
    end.
            

tunnel_connection(Port, Client, Backend, BackendsRest) ->
    io:format("Connecting to backend ~p...~n", [Backend]),
    {_Id, BackendData, _Timestamp} = Backend,
    case gen_tcp:connect(proplists:get_value(ip, BackendData), 
                                       list_to_integer(proplists:get_value(port, BackendData)), 
                                       ?TCP_OPTIONS) of
        {ok, BackendSock} ->
            try 
                send_receive(Client, BackendSock)
            catch 
                _:_ -> 
                    gen_tcp:close(Client)
            after 
                gen_tcp:close(BackendSock),
                update_lastused(Port, Backend)
            end;
        {error, _Reason} when BackendsRest == [] ->
            gen_tcp:close(Client);
        {error, _Reason} ->
            %% Retry with the next backend in list order
            [H | Rest] = BackendsRest,
            tunnel_connection(Port, Client, H, Rest)
    end.


send_receive(Client, BackendSock) ->
    inet:setopts(Client, [{active, once}]),
    receive
        {tcp, Client, ClientData} ->
            ok = gen_tcp:send(BackendSock, ClientData),
            inet:setopts(BackendSock, [{active, once}]),
            receive
                {tcp, BackendSock, BackendData} ->
                    ok = gen_tcp:send(Client, BackendData),
                    send_receive(Client, BackendSock);
                {tcp_closed, BackendSock} ->
                    ok
            end;
        {tcp_closed, Client} ->
            ok
    end.


load_balance(Backends, Nth, rr) 
  when Nth == length(Backends) ->
    {lists:nth(Nth, Backends), 1};
load_balance(Backends, Nth, rr) ->
    {lists:nth(Nth, Backends), Nth + 1};
load_balance(Backends, Nth, lru) -> 
    {hd(lists:keysort(3, Backends)), Nth}.


update_lastused(Port, {Id, Backend, _Timestamp}) ->
    [{Port, Backends}] = ets:lookup(tcp_route_backends, Port),
    ets:insert(tcp_route_backends, 
               {Port, lists:keyreplace(Id, 1, Backends, {Id, Backend, now()})}).
