-module(orderManager).
-compile(export_all).
-record(order, {floor, dir}).


%Initializing three different queues 
start() ->
	orderListInit(localOrders, process_local_queue),
	orderListInit(globalOrders, process_global_queue),
	orderListInit(privateGlobalOrders, process_private_global_queue),
	process_shell ! orderManagerInitialized.


orderListInit(Array, PID) ->
	io:format("[Order Manager] Initializing ~p~n", [Array]),
	case Array of
		localOrders ->
			dets:open_file(Array, [{type, bag}]),
			InitOrders = dets:lookup(Array, order),
			dets:close(Array),
			lists:foreach(fun(Order) -> driver:set_order_button_light(process_driver, cab, Order#order.floor, on) end, InitOrders);
		_Array ->
			InitOrders = []
	end,
	register(PID, spawn(fun() -> queue(InitOrders, Array) end)).


queue(Orders, Array) ->
	receive
		{add_order, NewOrder} ->
			case sets:is_element(NewOrder, sets:from_list(Orders)) of
				false ->
					UpdatedOrders = Orders ++ [NewOrder],
					case Array of
						localOrders ->
							dets:open_file(Array, [{type, bag}]),
							dets:insert(Array,NewOrder),
							dets:close(Array),
							process_eventHandler ! {newLocalOrders, UpdatedOrders},
							driver:set_order_button_light(process_driver, element(3,NewOrder),element(2,NewOrder), on);
						privateGlobalOrders ->
							process_eventHandler ! {newGlobalOrders, UpdatedOrders},
							watchDog:spawnWatchDog({NewOrder,node()}),
							lists:foreach(fun(Node) -> {process_global_queue, Node} ! {sync_global, NewOrder} end,[node()|nodes()]);
						globalOrders ->
							globalOrderDistribution(NewOrder)
						end,
					queue(UpdatedOrders, Array);
				true ->
					queue(Orders, Array)
			end;
		{remove_order, Order} ->
			case sets:is_element(Order, sets:from_list(Orders)) of
				true ->
					UpdatedOrders = Orders -- [Order],
					case Array of
						localOrders ->
							dets:open_file(Array, [{type, bag}]),
							dets:delete_object(Array, Order),
							dets:close(Array),
							process_eventHandler ! {newLocalOrders, UpdatedOrders};
						privateGlobalOrders ->
							process_eventHandler ! {newGlobalOrders, UpdatedOrders};
						globalOrders ->
							ok
						end,
						driver:set_order_button_light(process_driver, element(3,Order),element(2,Order), off),
						queue(UpdatedOrders, Array);
				false ->
						queue(Orders, Array)
					end;

		{get_orders, PID} ->
			PID ! {orders, Orders},
			queue(Orders, Array);
		{sync_global, NewOrder} ->
			case sets:is_element(NewOrder, sets:from_list(Orders)) of
				false ->
					UpdatedOrders = Orders ++ [NewOrder],
					driver:set_order_button_light(process_driver, element(3,NewOrder),element(2,NewOrder), on),
					queue(UpdatedOrders, Array);
				true ->
					driver:set_order_button_light(process_driver, element(3,NewOrder),element(2,NewOrder), on),
					queue(Orders, Array)
			end
	end.


%Fetch Orders from inputed queue
getOrders(Queue) ->
	Home = self(),
	Queue ! {get_orders, Home},
	receive
		{orders, Orders} ->
			Orders
	after 3000 ->
		io:format("[Order Manager] Order pull request TIMED OUT for ~p ~n", [Queue]),
		[]
	end.



%Framework for adding orders to call when a button is pressed
addOrder(Floor, Dir) ->
	NewOrder = #order{floor = Floor, dir = Dir},
	LocalOrders = getOrders(process_local_queue),
	GlobalOrders = getOrders(process_global_queue),
		case Dir of
			cab ->
				case sets:is_element (NewOrder, sets:from_list(LocalOrders)) of
					false ->
						process_local_queue ! {add_order, NewOrder};
					true ->
						ok
				end;
			_Other ->
				case sets:is_element(NewOrder, sets:from_list(GlobalOrders)) of
					false ->
						process_global_queue ! {add_order, NewOrder};
					true ->
						ok
				end

		end.



%Framework for removing orders to call when a floor is reached
removeOrder(Floor, SweepState) ->
	LocalOrders = getOrders(process_local_queue),
	GlobalOrders = getOrders(process_global_queue),
	PrivateGlobalOrders = getOrders(process_private_global_queue),

	case SweepState of
		sweepUp ->
			CheckOrderLocal = #order{floor = Floor, dir = cab},
			CheckOrderGlobal = #order{floor = Floor, dir = hall_up},
			case sets:is_element(CheckOrderLocal, sets:from_list(LocalOrders)) of
				true ->
					process_local_queue ! {remove_order, CheckOrderLocal};
				false ->
					ok
			end,
			case sets:is_element(CheckOrderGlobal, sets:from_list(PrivateGlobalOrders)) of
				true ->
					process_private_global_queue ! {remove_order, CheckOrderGlobal},
					watchDog:killWatchDog({CheckOrderGlobal, node()});
				false ->
					ok
			end,
			case sets:is_element(CheckOrderGlobal, sets:from_list(GlobalOrders)) of
				true ->
					lists:foreach(fun(Node) -> {process_global_queue, Node} ! {remove_order, CheckOrderGlobal} end, [node()|nodes()]);
				false ->
					ok

			end;
		sweepDown ->
			CheckOrderLocal = #order{floor = Floor, dir = cab},
			CheckOrderGlobal = #order{floor = Floor, dir = hall_down},
			case sets:is_element(CheckOrderLocal, sets:from_list(LocalOrders)) of
				true ->
					process_local_queue ! {remove_order, CheckOrderLocal};
				false ->
					ok
			end,
			case sets:is_element(CheckOrderGlobal, sets:from_list(PrivateGlobalOrders)) of
				true ->
					process_private_global_queue ! {remove_order, CheckOrderGlobal},
					watchDog:killWatchDog({CheckOrderGlobal, node()});
				false ->
					ok
			end,
			case sets:is_element(CheckOrderGlobal, sets:from_list(GlobalOrders)) of
				true ->
					lists:foreach(fun(Node) -> {process_global_queue, Node} ! {remove_order, CheckOrderGlobal} end, [node()|nodes()]);
				false ->
				ok

			end


	end.

%Sends order to the best available elevator
globalOrderDistribution(NewGlobalOrder) ->
	{Executor, _Others} = getBestElevator(NewGlobalOrder),
	io:format("[Order Manager] -> ~p is Executor ~n", [Executor]),
	case Executor of
		[] ->
			{process_private_global_queue, node()} ! {add_order, NewGlobalOrder};
		Executor ->
			{process_private_global_queue, element(1, Executor)} ! {add_order, NewGlobalOrder}
		end.

%Finds the best available elevator
getBestElevator(Order) ->  % Written by Tharald, student assistant
	{order, Floor, _Dir} = Order,
	Elevators = getEligableElevators(Order),
	TripCosts = lists:map(fun(Elevator) -> {abs(element(3, Elevator) - Floor), Elevator} end, Elevators),
	case TripCosts of
		[] ->
			Executor = [];
		TripCosts ->
    		[{_Cost, Executor} | _Rest] = lists:keysort(1, TripCosts)
    end,
    {Executor, Elevators--[Executor]}.



%Returns a list of all available elevators to handle order with current state. If no elevators are eligable, it returns it's own elevator
getEligableElevators(Order) ->
	ElevatorList = spawn(fun() -> listCreator([]) end),
	HomeNode = node(),
	lists:foreach(fun(Node) ->
		{process_eventHandler, Node} ! {getElevState, HomeNode},
		receive
			{elevatorState, SweepState, CurrentFloor, LengthOrderList} ->
				Elevator = {Node, SweepState, CurrentFloor},
				NodeCurrentFloor = CurrentFloor,
				TargetFloor = Order#order.floor,
				IsCurrentFloorOverTargetFloor = isFloorAOverFloorB(NodeCurrentFloor, TargetFloor),
				IsCurrentFloorUnderTargetFloor = isFloorAUnderFloorB(NodeCurrentFloor, TargetFloor),
				if
				    (NodeCurrentFloor == TargetFloor) ->
				        ElevatorList ! {add_elevator, Elevator};
				    (IsCurrentFloorOverTargetFloor and (SweepState == sweepDown)) ->
				    	ElevatorList ! {add_elevator, Elevator};
				    (IsCurrentFloorUnderTargetFloor and (SweepState == sweepUp)) ->
				    	ElevatorList ! {add_elevator, Elevator};
				    (LengthOrderList < 2) ->
				    	ElevatorList ! {add_elevator, Elevator};
				    true ->
				        ok
				end
			after 500 ->
				io:format("[Order Manager] -> ~p not available for orders ~n", [Node])
			end
	end, [node()|nodes()]),
	ElevatorList ! list_completed,
	Elevators = receive {completed_list, CandidateElevatorList} ->
			CandidateElevatorList
	after 500 ->
		[]
	end,
	case Elevators of
		[] ->
			process_eventHandler ! {getElevState, HomeNode},
			receive
				{elevatorState, LocalState, LocalFloor, _LengthOrderList} ->
					[{HomeNode, LocalState, LocalFloor}]
				end;
		Elevators ->
			Elevators
end.

listCreator(CandidateElevatorList) ->   % Written by Tharald, student assistant
	receive
		{add_elevator, Elevator} ->
			listCreator(CandidateElevatorList ++ [Elevator]);
		list_completed ->
			process_global_queue ! {completed_list, CandidateElevatorList}
		end.

%------------ self explanatory help functions ---------------


isFloorAOverFloorB(FloorA, FloorB) ->
    FloorA > FloorB.

isFloorAUnderFloorB(FloorA, FloorB) ->
    FloorA < FloorB.
