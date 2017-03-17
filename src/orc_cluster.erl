-module(orc_cluster).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).

-export([ start_link/1, stop/0, nodes/0, add/1, remove/1 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2 ]).

-record(orc_cluster, { nodes = [] }).

nodes() ->
	gen_server:call(?MODULE,nodes).

init([Nodes]) ->
	error_logger:info_msg("Cluster with nodes: ~p~n", [ Nodes ]),
	{ ok, #orc_cluster{ nodes = Nodes } }. 

add(Node) ->
	gen_server:call(?MODULE,{ add, Node }).

remove(Node) ->
	gen_server:call(?MODULE,{ remove, Node }).

start_link(Nodes) ->
	gen_server:start_link({ local,?MODULE },
		?MODULE,[Nodes],[]).

stop() ->
	gen_server:call(?MODULE,stop).

handle_call(nodes, _From, State = #orc_cluster{ nodes = Nodes }) ->
	{ reply, Nodes, State };

handle_call({ add, Node }, _From, State = #orc_cluster{ nodes = Nodes }) ->
	Rest = lists:delete(self(),Nodes),	
	{ Replies, BadNodes } = gen_server:multi_call( Rest, { add, Node }),
	error_logger:info_msg("Added ~p to ~p~n", [ Node, Replies ]),
	error_logger:error_msg("Failed to add ~p to ~p~n", [ Node, BadNodes ]),
	{ reply, ok, State };

handle_call({ remove, Node }, _From, State = #orc_cluster{ nodes = Nodes }) ->
	Rest = lists:delete(Node,Nodes),
	{ Replies, BadNodes } = gen_server:multi_call( Rest, ?MODULE, { remove, Node }),
	error_logger:info_msg("Removed ~p from ~p~n", [ Node, Replies ]),
	error_logger:error_msg("Failed to remove ~p to ~p~n", [ Node, BadNodes ]),
	{ reply, ok, State };

handle_call(stop, _From, State ) ->
	{ stop, stopped, State };

handle_call(Message, _From, State )->
	error_logger:error_msg("Unknown message ~p~n", Message ),
	{ reply, ok, State }.

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
