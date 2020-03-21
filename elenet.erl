-module(elenet).
-compile(export_all).

-define(BEACON_PORT, 45678).
-define(RADAR_PORT, 45679).




beacon() ->
	io:format("[BEACON] -> atempting broadcast to ~p from ~p ~n", [?RADAR_PORT, ?BEACON_PORT]),
	UdpState = gen_udp:open(?BEACON_PORT, [list, {active, true}, {broadcast, true}]),
	beacon(UdpState).

beacon({ok, BeaconSocket}) ->
	timer:sleep(1000 + rand:uniform(500)),
	UdpState = gen_udp:send(BeaconSocket, {255,255,255,255}, ?RADAR_PORT, atom_to_list(node())),
	beacon({UdpState, BeaconSocket});

beacon({error, Reason})->
		io:format("[BEACON] [ERROR] -> ~p - atempting reboot in 3 seconds ~n", [Reason]),
		timer:sleep(3000),
		beacon();

beacon({{error, Reason}, BeaconSocket}) ->
	gen_udp:close(BeaconSocket),
	beacon({error, Reason}).




radar() ->
	io:format("[RADAR] -> radar initiated and listening on port :~p~n", [?RADAR_PORT]),
	UdpState = gen_udp:open(?RADAR_PORT, [list, {active, false}]),
	radar(UdpState).


radar({ok, RadarSocket}) ->
		{ok, {_IPaddr, _Port, Node_encoded}} = gen_udp:recv(RadarSocket,0),
			Node = list_to_atom(Node_encoded),
			case lists:member(Node, [node()|nodes()]) of
				true ->
					radar({ok, RadarSocket});
				false ->
					net_adm:ping(Node),
					io:format("[RADAR] -> unique payload recieved from ~p, ~p, ~p, ~n", [_IPaddr, _Port, Node]),
					io:format("[RADAR] -> connections are ~p~n", [nodes()]),
					radar({ok, RadarSocket})
			end;


radar({error, Reason}) ->
			io:format("[RADAR] [ERROR] -> ~p - on message recieve. atempting again in 3 seconds ~n", [Reason]),
			timer:sleep(3000),
			radar().

sendToAll(Process, Message) ->
	lists:foreach(fun(Node) -> {Process, Node} ! Message end, nodes() ++ [node()]).
