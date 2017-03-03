#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa $(dirname 0)/ebin -noshell -noinput
%%
%% Copyright 2017 David J. Goehrig <dave@dloh.org>

%% starts a local node to issue commands from
%% we name these sequentially with system time
%% to avoid colliding with other command processes
connect(Server) ->
	net_kernel:start([ list_to_atom("cmd" ++ integer_to_list(erlang:system_time()) ++ "@localhost"), shortnames ]),
	case net_kernel:connect(Server) of
		false -> fail;
		true -> ok
	end.


%% process takes the command line arugments and performs
%% the associated action

%% cluster
process(_Server,["cluster","help"]) ->
	io:format("
orc cluster [stop|status|observer]

");

process(_Server,["cluster" | Command ]) ->
	Home = os:getenv("HOME"),
	Config = load_config(Home),
	Cluster = proplists:get_value(cluster,Config),
	[ process(Node,Command) || Node <- Cluster ];

%% init
process(_Server,["init","help"]) ->
	io:format("
orc [node] init

	This command initializes an Mnesia database on the given node.
	The database is used to store routes, users, file references,
	permissions, and REST interface objects.	

	The init command will read from your ~~/.orc file to discover
	{ cluster, [ nodes .. ] } config entry and initalize mnesia on
	all of the nodes.  Epmd must be running on all hosts prior to
	running the init command for it to work.

");

process(Server,["init"]) ->
	io:format("starting ~p~n", [ Server ]),
	{ok,_Pid} = net_kernel:start([ Server, shortnames ]),
	Home = os:getenv("HOME"),
	Config = load_config(Home),
	Cluster = proplists:get_value(cluster,Config),
	process_flag(trap_exit,true),
	Others = lists:delete(node(), Cluster),
	io:format("Connecting to ~p~n", [ Others ]),
	io:format("~p~n", [[ net_kernel:connect(Node) || Node <- Others ]]),
	rpc:multicall([node()|nodes()], application,load,[orc]),
	orc_database:initialize(Cluster),
	io:format("~p ", [ Server ]),
	green(ok),
	eol();

%% setup
process(_Server, [ "setup","help" ]) ->
	io:format("
orc setup file

");

process(Server, [ "setup", File ]) ->
	ok = connect(Server),
	{ ok, Config } = file:consult(File),
	io:format("loading config ~p~n", [ Config ]),
	Routes = proplists:get_value(routes, Config),
	[ rpc:call(Server, orc_router,add,[ Path, Module ]) ||  { Path, Module } <- Routes ],
	Files = proplists:get_value(files, Config),
	[ rpc:call(Server,orc_file,add,[ Path, Local, Mime ])  || { Path, Local, Mime } <- Files ],
	Users = proplists:get_value(users, Config),
	[ setup_user(Server, User, Email, Pass, Paths) ||
		{ User, Email, Pass, Paths } <- Users ],
	rpc:call(Server,mnesia,sync_log,[]),
	io:format("done~n");

%% status
process(_Server,["status","help"]) ->
	io:format("
orc [node] status
	
	The status command will print ok or fail while attempting
	to connect to the given node or each node in the cluster.
	This does not indicate that the node is actually down, 
	just that it was not able to connect to the node.

");

process(Server,["status"]) ->
	io:format("~p: ", [ Server ] ),
	case connect(Server) of
		ok -> green(ok);
		_ -> red(fail)
	end,
	eol();

%% observer
process(_Server,["observer","help"]) ->
	io:format("
orc [node] observer

	This command starts the observer on the given node.
	The Erlang observer is a standard gui for monitoring
	Erlang systems.

");

process(Server,["observer"]) ->
	ok = connect(Server),
	rpc:call(Server,observer,start,[]);

%% console
process(_Server,["console","help"]) ->
	io:format("
orc [node] console

	The console command connects to an erlang shell on the given node.
	It will take over the tty of the session and allows for total
	destruction of a node if you're into that sort of thing.

");

process(Server,["console"]) ->
	ok = connect(Server),
	process_flag(trap_exit,true),
	Shell = user_drv:start(['tty_sl -c -e',{Server,shell,start,[]}]),
	true = erlang:link(Shell),
	receive
		{ 'EXIT', Shell, _ } -> ok
	end,
	erlang:halt(0);

%% start
process(_Server,["start","help"]) ->
	io:format("
orc [node] start
	
	The start command will start a orc application on the given node.
	Epmd must be running on the host already for this command to work.
	Instructions for setting up epmd on boot can be found in the README.

");	

process(Server,["start"]) ->
	process_flag(trap_exit,true),
	case net_kernel:start([ Server, shortnames ]) of
		{ ok, Pid } ->
			io:format("Started ~p at ~p~n", [ Server, Pid ]),
			ok = connect(Server),
			rpc:call(Server,mnesia,start,[]),
			rpc:call(Server,application,load,[orc]),
			rpc:call(Server,orc,start,[]);
		{ error, Reason } ->
			io:format("Failed to start ~p: ~p~n", [ Server, Reason ])	
	end,
	receive
		'EXIT' -> 
			io:format("done~n"),
			erlang:halt(0)
	end;

%% stop
process(_Server,["stop","help"]) ->
	io:format("
orc [node] stop

	The stop command will stop a orc application on the given node.
	It will not stop epmd on the remote node, so you may call start 
	to start it again.

");

process(Server,["stop"]) ->
	case connect(Server) of
		ok ->
			rpc:call(Server, mnesia, stop, []),
			rpc:call(Server, orc, stop, []),
			rpc:call(Server, erlang, halt, [0]),
			green(ok),
			eol();
		fail ->
			red(fail),
			eol()
	end;
		
%% user
process(_Server,["user","help"]) ->
	io:format("
orc user [list|add|remove|grant|revoke]

	list				returns a list of all the usernames in the system
	add User Email Password		creates a new user with email and password
	auth Path User Password		test a user's auth credentials for a path
	remove User Email		removes the user with matching name and password
	grant User Path			grants a access to a path for a given user
	revoke User Path		removes access to a path for a given user

");

process(Server,["user","add",User,Email,Password]) ->
	ok = connect(Server),
	rpc:call(Server, orc_auth, add, [ User, Email, Password ]);

process(Server,["user","auth", Path, User, Password]) ->
	ok = connect(Server),
	Res = rpc:call(Server, orc_auth, test, [ Path, User, Password ]),
	io:format("~p~n", [ Res ]);

process(Server,["user","remove",User,Email]) ->
	ok = connect(Server),
	rpc:call(Server, orc_auth, remove,[ User, Email ]);

process(Server,["user","grant", User, Pattern ]) ->
	ok = connect(Server),
	rpc:call(Server, orc_auth, grant, [ User, Pattern ]);	

process(Server,["user","revoke", User, Pattern ]) ->
	ok = connect(Server),
	rpc:call(Server, orc_auth, revoke, [ User, Pattern ]);

process(Server,["user","list"]) ->
	ok = connect(Server),
	Users = rpc:call(Server, orc_auth, users, []),
	io:format("~p~n", [ Users ]);


%% This needs to be the last state in the process funciton, as it
%% is a catchall that prints out the status message.  If you are
%% adding new methods to it please add them above, and amend this
%% to reflect the new usage.
process(_Server,_) ->
	io:format("
usage: orc [cluster|node] [start|stop|status|console|observer|init|user]

	help 				- this message

	command help			- help for the given command

	cluster command			- runs a command on all nodes in a cluster

	start [node]			- start a new node

	stop [node]			- stop a node

	status [node]			- return the status of a node

	console [node]			- connects a console to a node

	observer [node]			- run the observer on a node

	init [node] 			- initialize orc database

	setup [node] file		- load a setup file on a node

	user [node] [list|add|remove|grant|revoke]
		list			- list all users by name
		add user email pass	- add a user
		remove user email	- remove a user
		grant user path		- grant access to a path
		revoke user path	- remove access to a path

").

%% Loads a configuration file from our home base
%%
load_config(Home) ->
	case file:consult(Home ++ "/.orc") of 
		{ok, Config } -> 
			Config;
		_ ->
			io:format("Please create a ~s/.orc configuration file~n", [ Home ]), halt(0)
	end.

set_cookie(Config) ->
	case proplists:get_value(cookie,Config) of
		undefined ->
			ok;
		Cookie ->
			erlang:set_cookie(node(),Cookie)
	end.

setup_user(Server,User,Email,Password,Paths) ->
	rpc:call(Server, orc_auth, add, [ User, Email, Password ]),
	[ rpc:call(Server, orc_auth, grant, [ User, Path ]) ||
		Path <- Paths ].

find_server([]) ->
	none;
find_server([Host|_Args]) ->
	case string:tokens(Host,"@") of
		[ _Name , _Host ] -> Host;
		_ -> none
	end.

find_host(Config,[])->
	Host = proplists:get_value(host,Config),
	case Host of
		undefined ->
			io:format("Please set host in ~~/.botbop~n"), 
			halt(1);
		Host ->
			{ Host, [] }
	end;
find_host(Config,[ _A | As ] = Args) ->
	Server = find_server(Args),
	case { Server, proplists:get_value(host,Config) } of
		{ none, undefined } ->
			io:format("Please set host in ~~/.orc~n"), 
			halt(1);
		{ Server, undefined } ->
			{ list_to_atom(Server), As };
		{ none, Host } ->
			{ Host, Args };
		{ Server, _Host } ->
			{ list_to_atom(Server), As }
	end.

eol() ->
	io:format("~n").

green(Term) ->
	io:format([ 16#1b | "[;32m"]),
	io:format("~p", [ Term ]),
	io:format([ 16#1b | "[;39m" ]).

red(Term) ->
	io:format([ 16#1b | "[;31m"]),
	io:format("~p", [ Term ]),
	io:format([ 16#1b | "[;39m" ]).

main(Args) ->
	Home = os:getenv("HOME"),
	Config = load_config(Home),
	{ Host, Args2 } = find_host(Config,Args),
	ok = set_cookie(Config),
	process(Host,Args2).
