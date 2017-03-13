-module(orc_routes).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).

-record(orc_routes, { pid, path, time, active = true }).

-export([load/0, add/2, remove/1, install/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

load() ->
	mnesia:activity(transaction, fun() ->
		mnesia:match_object(#orc_routes{ pid = '_', path = '_', time = '_', active = true })
	end).

add(Pid, Path) ->
	Time = os:system_time(),
	ok = mnesia:activity(transaction, fun() ->
		mnesia:write(#orc_routes{	
			pid = Pid,
			path = Path,
			time = Time,
			active = true
		})
	end),
	load().

remove(Pid) ->
	ok = mnesia:activity(transaction, fun() ->
		mnesia:delete(orc_routes, Pid, write)
	end),
	load().

	
install(Nodes) ->
	{ atomic, ok } = mnesia:create_table(orc_routes, [
		{ attributes, record_info(fields,orc_routes) },
		{ ram_copies, Nodes }]).

