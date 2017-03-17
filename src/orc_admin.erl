-module(orc_admin).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).

-behavior(gen_server).
-export([ start_link/0, stop/0 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).


-record(orc_admin, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
start_link() ->
	gen_server:start_link(?MODULE, #orc_admin{}, []).

stop() ->
	gen_server:call(?MODULE,stop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API

init( Admin = #orc_admin{} ) ->
	{ ok, Admin }.


handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", [ Message ]),
	{ noreply, State }.

terminate(Reason,_State) ->
	error_logger:info_msg("Shutting down orc_admin ~p~n", [ Reason ]),
	ok.

code_change(_Old,State,_Extra) ->
	{ ok, State }.
