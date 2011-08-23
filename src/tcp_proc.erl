-module(tcp_proc).
-export([start_link/1]).
-export([init/2]).
-export([tunnel_connection/2]).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).


start_link(Port) ->
    proc_lib:start_link(?MODULE, init, [self(), Port]).


init(Parent, Port) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
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
            BackendsLen = length(Backends),
            if 
                BackendsLen == 0 ->
                    loop(Port, Socket, 1);
                Nth == BackendsLen -> 
                    {_Id, Backend} = lists:nth(Nth, Backends),
                    proc_lib:spawn(?MODULE, tunnel_connection, [Client, Backend]),
                    loop(Port, Socket, 1);
                true ->
                    {_Id, Backend} = lists:nth(Nth, Backends),
                    proc_lib:spawn(?MODULE, tunnel_connection, [Client, Backend]),
                    loop(Port, Socket, Nth + 1)
            end;
        {error, Reason} ->
            error_logger:info_msg("Failed to accept connection ~p", [Reason]),
            loop(Port, Socket, Nth)
    end.
            

tunnel_connection(Client, Backend) ->
    io:format("Connecting to backend ~p...~n", [Backend]),
    case gen_tcp:connect(proplists:get_value(ip, Backend), 
                                       list_to_integer(proplists:get_value(port, Backend)), 
                                       ?TCP_OPTIONS) of
        {ok, Socket} ->
            try 
                send_receive(Client, Socket)
            catch 
                _:_ -> 
                    gen_tcp:close(Client)
            after 
                gen_tcp:close(Socket)
            end;
        {error, _Reason} ->
            %% @todo Try to connect to the next backend instead!
            gen_tcp:close(Client)
    end.


send_receive(Client, Socket) ->
    case gen_tcp:recv(Client, 0) of
        {ok, B1} ->
            ok = gen_tcp:send(Socket, B1),
            {ok, B2} = gen_tcp:recv(Socket, 0),
            ok = gen_tcp:send(Client, B2),
            send_receive(Client, Socket);
        {error, closed} ->
            ok
    end.    
            
