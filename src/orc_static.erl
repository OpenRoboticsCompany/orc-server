-module(orc_static).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J. Goehrig"/utf8>>).
-export([ get/1, static_file/1, default_file/1, file/1 ]).

-include("include/orc_http.hrl").

get(Request = #request { path = Path }) ->	
	case orc_auth:auth(Request) of 
		#request{} ->
			error_logger:info_msg("Authorized ~p~n", [ Path ]),
			static_file(Request);
		Response = #response{} ->
			error_logger:error_msg("Authentication failure ~p~n", [ Request ]),
			Response
	end.


default_file(#request{ path = Path, headers = Headers }) ->
	Host = proplists:get_value(<<"Host">>, Headers),
	PathBin = binary:list_to_bin(Path),
	<<"<script>ws = new WebSocket('wss://", Host/binary, PathBin/binary,
		"','json'); ws.onmessage = function(msg) {"
		" console.log(JSON.parse(msg.data)) };</script>">>.

file(Path) ->
	Filename = code:priv_dir(orc) ++ "/html" ++ Path,
	error_logger:info_msg("Looking for file ~p~n",[ Filename ]),
	file:read_file(Filename).

static_file(Request = #request{ path = Path }) ->
	Content = case file(Path) of
		{ ok, Bin } ->
			Bin;
		{ error, Error } ->
			erro_logger:error_msg("Returning default ~p~n", [ Error ]),
			default_file(Request)
	end,
	ContentLength = binary:list_to_bin(integer_to_list(byte_size(Content))),
	#response{ 
		socket = Request#request.socket,
		upgrade = false,
		status = 200,
		protocol = <<"HTTP/1.1">>,
		headers = [
			{ <<"Content-Length">>, ContentLength },
			{ <<"Content-Type">>, <<"text/html">> }
		],
		body = Content
	}.
