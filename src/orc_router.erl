-module(orc_router).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).

%% router behavior
-export([ start_link/0, stop/0, connect/2, route/1, close/1 ]).

%% gen_server behavior
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-include("include/orc_http.hrl").
-record(orc_router, { paths = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link() ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE,stop).

connect(Pid,Path) ->
	error_logger:info_msg("Connect Adding ~p for ~p~n", [ Path, Pid]),
	gen_server:cast(?MODULE,{ connect, Path, Pid }).

close(Pid) ->
	gen_server:cast(?MODULE, { close, Pid }).


route(<<>>) ->
	error_logger:info_msg("Attempt to route empty message~n"),
	ok;
route(Data) ->
	error_logger:info_msg("Routing ~p~n", Data ),
	gen_server:abcast(orc_cluster:nodes(), ?MODULE, { route, Data }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([]) ->
	{ ok, #orc_router{ paths = orc_routes:load() }}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ reply, #response{ status = 405 }, State }.

handle_cast({ connect, Path, Pid }, State) ->
	error_logger:info_msg("Adding ~p for ~p~n", [ Path, Pid ]),
	Paths = orc_routes:add(Pid,Path),
	{ noreply, State#orc_router{ paths = Paths } };	

handle_cast({ route, Data }, State = #orc_router{ paths = Paths }) ->
	error_logger:info_msg("Paths are ~p~n", [ Paths ]),
	[ orc_websocket:send(Pid, Data) || Pid <- orc_path:scan(Data,Paths) ], 	
	{ noreply, State };

handle_cast({ close, Pid }, State ) ->
	error_logger:info_msg("Adding ~p ~n", [ Pid ]),
	Paths = orc_routes:remove(Pid),
	{ noreply, State#orc_router{ paths = Paths }};

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.
