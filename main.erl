-module(main).
-compile(export_all).

-define(Cookie, 'LiteralShit').
-define(NODE_TIMEOUT, 12).
-define (NUM_FLOORS, 4).

c() ->
	compile:file("driver.erl"),
	compile:file("poller.erl"),
	compile:file("fsm.erl"),
	compile:file("eventHandler.erl"),
	compile:file("elenet.erl"),
	compile:file("watchDog.erl"),
	compile:file("orderManager.erl"),
	compileDone.

start()->
	% check if what OS the system is to run appropriate terminal commands
	{OsFamily, OsType} = os:type(),
	io:format("[main] -> starting elevator~n"),
	io:format("[main] -> os recognised as ~p~n", [{OsFamily, OsType}]),

	% get the local IP-address and parse it to a string
	{ok, [{IPtuple,_Broadcast, _Self} | _Disregard]} = inet:getif(),
	NodeName = "elevator@"++inet_parse:ntoa(IPtuple),
	start(OsFamily, NodeName).

start(win32, NodeName) ->
	% run init in a new terminal on windows
	os:cmd("del \"globalOrders\""),
	spawn( fun() -> os:cmd("start cmd /k erl -run main init -name "++NodeName) end);

start(unix, NodeName) ->
	% run init in same terminal on unix
	NodeAtom = list_to_atom(NodeName),
	os:cmd("epmd -daemon"),
	os:cmd("rm globalOrders"),
	os:cmd("rm privateGlobalOrders"),
	net_kernel:start([NodeAtom, longnames, 500]),
	init().

init()->
	%% Necessary system values are set_cookie
	erlang:set_cookie(node(), ?Cookie),
	net_kernel:set_net_ticktime(4, 0),
	register(process_shell, self()),

	%% Spawn elevator processes and register
	_BeaconPid = spawn(fun() -> elenet:beacon() end),
	% No registry required
	_RadarPid = spawn(fun() -> elenet:radar() end),
	% No registry required

	FlusherPid = spawn(fun() -> flusher() end),
	register(process_flusher, FlusherPid),


	{ok, DriverPid} = driver:start(),
	register(process_driver, DriverPid),
	
	OrderManagerPid = spawn(fun() -> orderManager:start() end),
	register(process_orderManager, OrderManagerPid),

        receive
                orderManagerInitialized ->
	                EventHandlerPid = spawn(fun() -> eventHandler:start() end),
	                register(process_eventHandler, EventHandlerPid)
	end,
	

	FsmPid = spawn(fun() -> fsm:start() end),
	register(process_fsm, FsmPid),
	
	
	PollerPid = spawn(fun() -> poller:start() end),
	register(process_poller, PollerPid),

	WatchDogPID = spawn(fun() -> watchDog:start() end),
	register(process_watchDog, WatchDogPID),

	io:format("nyello~n").


flusher() ->
	receive
		Anything ->
			io:format("[FLUSHER] -> ~p~n", [Anything]),
			flusher()
	end.
