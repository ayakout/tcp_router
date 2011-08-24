%%% -*- mode:erlang -*-
{application, tcp_router,
 [{description, "TCP ROUTER"},
  {vsn, "0.1"},
  {modules,[tcp_router, mod_tcprouter, tcp_proc_sup, tcp_proc]},
  {registered, []},
  {applications,[kernel,stdlib,inets]},
  {mod, {tcp_router, []}},
  {env, [{load_balance, lru}]} %% rr | lru
 ]      
}.