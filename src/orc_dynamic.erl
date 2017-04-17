-module(orc_dynamic).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2017 David J. Goehrig"/utf8>>).
-export([ get/1, start_link/0, mount/2 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-behavior(gen_server).

-include("include/orc_http.hrl").
-record(orc_dynamic, { routes = [] }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Interface

start_link() ->
	gen_server:start_link({ local,?MODULE }, ?MODULE, [], []).

get(Request = #request { path = Path }) ->	
	case orc_auth:auth(Request) of 
		#request{} ->
			error_logger:info_msg("Authorized ~p~n", [ Path ]),
			Content = gen_server:call(?MODULE,{ get, Request }),
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
			};
		Response = #response{} ->
			error_logger:error_msg("Authentication failure ~p~n", [ Request ]),
			Response
	end.

mount(Path,Module) ->
	gen_server:cast(?MODULE,{ mount, Path, Module }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Interface

init([]) ->
	ModuleDir = code:priv_dir(orc) ++ "/modules/",
	{ ok, Dir } = file:list_dir(ModuleDir),
	Files = lists:usort(lists:map ( fun (F) -> lists:nth(1,string:tokens(F,".")) end, Dir)),
	Modules = [ code:load_abs(ModuleDir ++ File) || File <- Files ],
	error_logger:info_msg("Loaded ~p~n", [ Modules ]),
	[ Module:init() || { module, Module } <- Modules ],
	{ ok, #orc_dynamic{ routes = [] }}.

handle_call({ get, Request = #request{ path = Path }}, _From, State = #orc_dynamic{ routes = Routes }) ->
	case proplists:get_value(Path,Routes) of
		undefined -> 
			{ reply, orc_static:get(Request), State };
		Module ->
			{ reply, Module:get(Request), State }
	end;	

handle_call(Message,_From,State) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ reply, ok, State }.

handle_cast({ mount, Path, Module }, State = #orc_dynamic{ routes = Routes }) ->
	error_logger:info_msg("Routing ~p -> ~p~n", [ Path, Module ]),	
	{ noreply, State#orc_dynamic { routes = [ { Path, Module } | Routes ] }};

handle_cast(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	error_logger:error_msg("Unknown message ~p~n", 	[ Message ]),
	{ noreply,  State }.

terminate(Reason,_State) ->
	error_logger:info_msg("orc_dynamic shutting down ~p~n", [ Reason ]),
	ok.

code_change(_Old,_Extra,State) ->
	{ ok, State }.




