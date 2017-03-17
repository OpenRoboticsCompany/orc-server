-module(orc).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start/0, start_link/1, stop/0, server/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-include("include/orc_http.hrl").
-record(orc, { socket, request = #request{} }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

%% create a orc_server under supervision for the port
server(Port) ->
	orc_sup:server(Port).

start() ->
	application:ensure_all_started(orc).

start_link(Socket) ->
	gen_server:start_link(?MODULE, #orc{ 
		socket = Socket,
		request = #request{}
	},[]).

stop() ->
	application:stop(orc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init(Orc = #orc{ socket = Listen }) ->
	case ssl:transport_accept(Listen) of
		{ ok, Socket } ->
			case ssl:ssl_accept(Socket,1000) of
				ok ->
					{ ok, Orc#orc{
						socket = Socket,
						request = #request{ socket = Socket }
					}};
				{ error, timeout } ->
					error_logger:error_msg("SSl timeout~n"),
					{ stop, no_ssl };
				{ error, Reason } ->
					error_logger:error_msg("SSL connection failed: ~p", [ Reason ]),
					{ stop, Reason }
			end;
		{ error, Reason } ->
			error_logger:error_msg("Socket accept failed: ~p", [ Reason ]),
			{ stop, Reason }
	end.
	
handle_call(Message,_From,Orc) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ reply, ok, Orc }.

handle_cast(stop,State) ->
	error_logger:info_msg("Stopping"),
	{ stop, stopped, State };

handle_cast(Response = #response{},Orc = #orc{ socket = Socket }) ->
	Bin = orc_http:response(Response),
	ssl:send(Socket,Bin),
	{ noreply, Orc };

handle_cast(Message,Orc) ->
	error_loggger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Orc }.

handle_info({ ssl, Socket, Data }, Orc = #orc{ request = Req }) ->
	Request = orc_http:request(Req,Data),
	case Request#request.stage of
		done ->
			handle_request(Request),
			{ noreply, Orc#orc{ request = #request{ socket = Socket } }};
		_ ->
			{ noreply, Orc#orc{ request = Request }}
	end;

handle_info({ ssl_closed, _Socket}, Orc = #orc{}) ->
	error_logger:info_msg("connection closed"),
	{ stop, normal, Orc };

handle_info(Message,Orc) ->
	error_logger:error_msg("Unknown message: ~p", [ Message ]),
	{ noreply, Orc }.

terminate(_Reason,#orc{ socket = Socket }) ->
	ssl:close(Socket),
	ok.

code_change( _Old, Orc, _Extra) ->
	{ ok, Orc }.


handle_request(Request = #request{headers = Headers, socket = Socket }) ->
	case proplists:get_value(<<"Upgrade">>, Headers) of
		<<"websocket">> ->
			Response = orc_websocket:get(Request),
			Bin = orc_http:response(Response),
			ssl:send(Socket,Bin);
		_ ->
			Response = orc_static:get(Request),
			Bin = orc_http:response(Response),
			ssl:send(Socket,Bin),
			ssl:close(Socket)
	end.

