-module(tcp_proc_sup).
-behaviour(supervisor).
-export([start_proc/1]).
-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_proc(Port) ->
    supervisor:start_child(?MODULE, [Port]).


init([]) ->
    TcpProc = {tcp_proc, {tcp_proc, start_link, []}, 
               transient, brutal_kill, worker, [tcp_proc]},
    {ok, {{simple_one_for_one, 10, 1}, [TcpProc]}}.
