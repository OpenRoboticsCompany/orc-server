-module(orc_cluster).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).

-export([ start_link/0, stop/0, broadcast/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2 ]).

-record(orc_cluster, { nodes = [] }).

init([]) ->
	{ ok, Config } = file:consult(os:getenv("HOME") ++ "/.orc"),
	Nodes = proplists:get_value(cluster,Config),
	{ ok, #orc_cluster{ nodes = Nodes } }. 

start_link() ->
	gen_server:start_link({ local,?MODULE },
		?MODULE,[],[]).

stop() ->
	gen_server:call(?MODULE,stop).

broadcast(Message) ->
	gen_server:cast(?MODULE, { broadcast, Message }).


handle_call(stop, _From, State ) ->
	{ stop, stopped, State };

handle_call(Message, _From, State )->
	error_logger:error_msg("Unknown message ~p~n", Message ),
	{ reply, ok, State }.

handle_cast({broadcast,Message},State = #orc_cluster{ nodes = Nodes }) ->
	[ Node ! Message || Node <- Nodes ],
	{noreply, State };

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", Message ),
	{ noreply, State }.
	
handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", Message ),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.
