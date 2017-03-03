-module(orc_path).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016,2017 David J Goehrig, Open Robotics Company LLC."/utf8>>).

-export([ match/2, scan/2, components/1, eval/2, validate/2  ]).

-record(orc_routes, { pid, path, time, active = true }).


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
	case lists:member(binary:list_to_bin(K),proplists:get_keys(Data)) of
		true ->	compare(Data, Tail );
		_ -> false
	end;
compare(Data, [ { K, V } | Tail]) ->
	case lists:member({binary:list_to_bin(K),binary:list_to_bin(V)},Data) of
		true -> compare(Data,Tail);
		_ -> false
	end.
	
%% searches a proplist for a path match
%% { Pattern, Pid }
scan(Data, Paths) ->
	[ V || #orc_routes{ pid = V } <- lists:filter( fun(#orc_routes{path = K}) ->
		M = match(Data,K),
		error_logger:info_msg("Path: ~p Matches: ~p Data: ~p~n", [ K, M, Data ]),
		M
	 end, 
	Paths) ].

%% compares a path vs a path pattern
eval(Path,Pattern) ->
	error_logger:info_msg("~p vs ~p~n", [Path, Pattern]),
	PathSegments = components(Path),
	PatternSegments = components(Pattern),
	validate(PathSegments,PatternSegments).

validate([],[]) ->
	error_logger:info_msg("validate true~n"),
	true;
validate([{KA,VA}|TA],[{KB,VB}|TB]) ->
	error_logger:info_msg("A ~p vs B ~p~n", [ KA, KB ]),
	case (KB =:= "*") 
		or ((KA =:= KB) 
			and ((VA =:= VB) 
			or (VB =:= "*"))) of
		true -> validate(TA,TB);
		false -> false
	end;
validate([],[{"*","*"}|_TB]) ->
	true;
validate(_,_) ->
	error_logger:info_msg("validate false~n"),
	false.
	
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
