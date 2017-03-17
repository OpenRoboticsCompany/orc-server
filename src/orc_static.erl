-module(orc_static).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J. Goehrig"/utf8>>).
-export([ get/1 ]).

-include("include/orc_http.hrl").

get(Request = #request { path = Path, headers = Headers }) ->	
	case orc_auth:auth(Request) of 
		#request{} ->
			error_logger:info_msg("Authorized ~p~n", [ Path ]),
			Host = proplists:get_value(<<"Host">>, Headers),
			PathBin = binary:list_to_bin(Path),
			Body =  <<"<script>ws = new WebSocket('wss://", Host/binary, PathBin/binary,
					"','json'); ws.onmessage = function(msg) {"
					" console.log(JSON.parse(msg.data)) };</script>">>,
			ContentLength = binary:list_to_bin(integer_to_list(byte_size(Body))),
			#response{ 
				socket = Request#request.socket,
				upgrade = false,
				status = 200,
				protocol = <<"HTTP/1.1">>,
				headers = [{ <<"Content-Length">>, ContentLength }],
				body = Body
			};
		Response = #response{} ->
			error_logger:error_msg("Authentication failure ~p~n", [ Request ]),
			Response
	end.
