-module(orc_sup).
-behaviour(supervisor).
-export([start_link/0, server/2]).
-export([init/1]).


-define(WEBPAGE_SERVER(P), list_to_atom("orc_server_" ++ integer_to_list(P))).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ ok, Config } = file:consult(os:getenv("HOME") ++ "/.orc"),
	Nodes = proplists:get_value(cluster,Config),
	{ok, { {one_for_one, 5, 10}, [
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
		]}
	]}}.

server(Module,Port) ->
	supervisor:start_child(?MODULE, #{ 
		id =>  ?WEBPAGE_SERVER(Port),
		start => { orc_server, start_link, [ Module, Port ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			orc_server,
			orc
		]}).
