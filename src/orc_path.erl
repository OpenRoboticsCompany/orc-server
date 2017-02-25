-module(orc_path).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016,2017 David J Goehrig, Open Robotics Company LLC."/utf8>>).

-export([ match/2, scan/2, components/1 ]).

segments([K,V]) ->
	{ K, V };
segments([K]) ->
	{ K, "*" };
segments(_) ->
	{}.

components(Path) ->
	Segments = string:tokens(Path,"/"),
	[ segments(string:tokens(Segment,"=")) || Segment <- Segments ].

match(Data,Path) ->
	Pattern = components(Path), 
	compare(Data,Pattern).

%% content sensitive matching
compare(_Data,[]) -> true;	
compare(_Data,[{}]) -> true;
compare(_Data,[ { "*", "*"} | _Tail ]) -> true;
compare(Data, [ { K, "*" } | Tail ]) ->
	case lists:member(K,proplists:get_keys(Data)) of
		true ->	compare(Data, Tail );
		_ -> false
	end;
compare(Data, [ { K, V } | Tail]) ->
	case lists:member({K,V},Data) of
		true -> compare(Data,Tail);
		_ -> false
	end.
	
%% searches a proplist for a path match
%% { Pattern, Pid }
scan(Data, Paths) ->
	[ V || { _K, V } <- lists:filter( fun({K,_V}) ->
		match(Data,K) end, 
	Paths) ].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
match_test() ->
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/")),
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/*")),
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo/*")),
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo/narf")),
	?assertEqual(false, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo/narf/blat")),
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo/narf=blat")),
	?assertEqual(false, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo=blat/narf=blat")),
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo=bar/narf=")),
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo=bar/narf=*")),
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo=bar/narf")),
	?assertEqual(true, orc_path:match( [ {"foo","bar"},{"narf","blat"}], "/foo=bar/*/narf=borf")).

-endif.
