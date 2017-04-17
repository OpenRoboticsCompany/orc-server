-module(orc_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


-define(ORC_SERVER(P), list_to_atom("orc_server_" ++ integer_to_list(P))).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ ok, Port} = application:get_env(orc,port),
	{ ok, Nodes } = application:get_env(orc,cluster),
	{ok, { {one_for_one, 5, 10}, [
		#{ id => orc_cluster,
		start => { orc_cluster, start_link, [Nodes]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			orc_cluster
		]},
		#{ id => orc_database,
		start => { orc_database, start_link, [ Nodes ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			orc_database
		]},
		#{ id => orc_websocket_sup,
		start => { orc_websocket_sup, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => supervisor,
		modules => [
			orc_websocket_sup
		]},
		#{ id => orc_router,
		start => { orc_router, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			orc_router
		]},
		#{ id => orc_dynamic,
		start => { orc_dynamic, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			orc_dynamic,
			orc_static
		]},
		#{ id => orc_admin,
		start => { orc_admin, start_link, []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [
			orc_admin
		]},
		#{ id => ?ORC_SERVER(Port),
		start => { orc_server, start_link, [ Port ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			orc_server,
			orc
		]}
	]}}.
