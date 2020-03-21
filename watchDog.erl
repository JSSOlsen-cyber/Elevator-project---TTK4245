-module(watchDog).
-compile(export_all).

-define(DOGE_TIMEOUT, 30000).



watchDog(WatchDogServer, Order, Timeout) ->
	receive
		{kill, Order} ->
			io:format("[watchDog] ~p -> watchDog is dead: ~p~n", [self(),Order]),
			ThisWatchDogPID = self(),
			WatchDogServer ! {is_killed, ThisWatchDogPID},
			exit(self(), normal)
	after
		Timeout ->
			ThisWatchDogPID = self(),
			WatchDogServer ! {ThisWatchDogPID, timeout, Order},
			io:format("[watchDog] ~p -> watchdog is timed out: ~p~n", [self(),Order]),
			exit(self(), normal)
	end.



watchDogServer(CurrentWatchDogs) ->
	%io:format("[watchDog] -> doges remaining are: ~p~n", [CurrentWatchDogs]),
	receive
		{is_killed, WatchDogPID} ->
			%io:format("[doge_server] -> doge registered killed in doge_server: ~p~n", [DogePID]),
			NewWatchDogs = lists:delete(WatchDogPID, CurrentWatchDogs),
			watchDogServer(NewWatchDogs);
		{kill, Order} ->
			%io:format("[doge_server] -> doge is to be killed: ~p~n", [Data]),
			lists:foreach(fun(WatchDogPID) -> WatchDogPID ! {kill, Order} end, CurrentWatchDogs),
			watchDogServer(CurrentWatchDogs);
		{start, Order} ->
			WatchDogServer = self(),
			NewWatchDog = spawn(fun() -> watchDog(WatchDogServer, Order, ?DOGE_TIMEOUT) end),
			io:format("[watchDog] -> new doge created: ~p~p~n", [NewWatchDog,Order]),
			watchDogServer([NewWatchDog|CurrentWatchDogs]);
		{WatchDogPID, timeout, Order} ->
			NewWatchDogs = lists:delete(WatchDogPID, CurrentWatchDogs),
			%io:format("[doge_server] -> doge timed out in doge_server: ~p~n", [Data]),
			%do something
			{TimedOutOrder, OrderNode} = Order,
			case lists:member(OrderNode, nodes()) of
				true ->
					disconnect_node(OrderNode);
					%ok;
				false ->
					ok
				end,
			process_global_queue ! {remove_order, TimedOutOrder},
			process_global_queue ! {add_order, TimedOutOrder},
			watchDogServer(NewWatchDogs);
		Unknown ->
			io:format("[watchDog] -> UNKNOWN MESSAGE ~p~n", [Unknown]),
			watchDogServer(CurrentWatchDogs)
	end.



start() ->
	io:format("[watchDog] -> watchDog initialized~n"),
	watchDogServer([]).

spawnWatchDog(Order) ->
	elenet:sendToAll(process_watchDog, {start, Order}).

killWatchDog(Order) ->
	elenet:sendToAll(process_watchDog, {kill, Order}).



%░░░░░░░█▐▓▓░████▄▄▄█▀▄▓▓▓▌█
%░░░░░▄█▌▀▄▓▓▄▄▄▄▀▀▀▄▓▓▓▓▓▌█ Such concurrency
%░░░▄█▀▀▄▓█▓▓▓▓▓▓▓▓▓▓▓▓▀░▓▌█
%░░█▀▄▓▓▓███▓▓▓███▓▓▓▄░░▄▓▐█▌
%░█▌▓▓▓▀▀▓▓▓▓███▓▓▓▓▓▓▓▄▀▓▓▐█ Many threads
%▐█▐██▐░▄▓▓▓▓▓▀▄░▀▓▓▓▓▓▓▓▓▓▌█▌
%█▌███▓▓▓▓▓▓▓▓▐░░▄▓▓███▓▓▓▄▀▐█
%█▐█▓▀░░▀▓▓▓▓▓▓▓▓▓██████▓▓▓▓▐█
%▌▓▄▌▀░▀░▐▀█▄▓▓██████████▓▓▓▌█▌ wow
%▌▓▓▓▄▄▀▀▓▓▓▀▓▓▓▓▓▓▓▓█▓█▓█▓▓▌█▌
%█▐▓▓▓▓▓▓▄▄▄▓▓▓▓▓▓█▓█▓█▓█▓▓▓▐█
