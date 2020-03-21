-module (eventHandler).
-compile(export_all).


start() ->
    InitLocalOrders=orderManager:getOrders(process_local_queue),
    eventHandler(InitLocalOrders, [], sweepUp, 0).

eventHandler(LocalOrders,GlobalOrders, SweepState, CurrentFloor) ->
    CurrentOrders = LocalOrders ++ GlobalOrders,
    LengthOrderList = length(GlobalOrders),

    receive
        {getElevState, HomeNode} ->
            {process_global_queue, HomeNode} ! {elevatorState, SweepState, CurrentFloor, LengthOrderList},
            eventHandler(LocalOrders,GlobalOrders, SweepState, CurrentFloor);

        {newLocalOrders, NewLocalOrders} ->
            io:format("[eventHandler] -> New Local orders recieved ~p~n", [NewLocalOrders]),
            eventHandler(NewLocalOrders, GlobalOrders, SweepState, CurrentFloor);

        {newGlobalOrders, NewGlobalOrders} ->
            io:format("[eventHandler] -> New global orders recieved ~p~n", [NewGlobalOrders]),
            eventHandler(LocalOrders, NewGlobalOrders, SweepState, CurrentFloor);

        {reachedFloor, FloorEntered} ->
            StopOnThisFloor = shouldIStopOnThisFloor(CurrentOrders, SweepState, FloorEntered),
            case StopOnThisFloor of
                true ->
                    io:format("[eventHandler] -> Stopping on this floor ~p~n", [FloorEntered]),
                    process_fsm ! {stopOnThisFloor, FloorEntered},
                    orderManager:removeOrder(FloorEntered, SweepState);
                false ->
                    ok
            end,
            eventHandler(LocalOrders, GlobalOrders, SweepState, CurrentFloor);

        {reachedTargetFloor, FloorReached} ->
            ApexFloorUp = getApexFloor(CurrentOrders, FloorReached, sweepUp),
            ApexFloorDown = getApexFloor(CurrentOrders, FloorReached, sweepDown),
            AreThereOrdersOnThisFloor = areThereOrdersOnThisFloor(CurrentOrders, FloorReached),
            case SweepState of
                sweepUp ->
                    if
                        ApexFloorUp /= noApex->
                            process_fsm ! {targetFloor, ApexFloorUp},
                            orderManager:removeOrder(FloorReached, sweepUp),
                            eventHandler(LocalOrders, GlobalOrders, sweepUp, FloorReached);
                        ApexFloorDown /= noApex->
                            process_fsm ! {targetFloor, ApexFloorDown},
                            orderManager:removeOrder(FloorReached, sweepUp),
                            orderManager:removeOrder(FloorReached, sweepDown),
                            eventHandler(LocalOrders, GlobalOrders, sweepDown, FloorReached);
                        AreThereOrdersOnThisFloor ->
                            process_fsm ! {openDoors},
                            orderManager:removeOrder(FloorReached, sweepUp),
                            orderManager:removeOrder(FloorReached, sweepDown),
                            eventHandler(LocalOrders, GlobalOrders, sweepUp, FloorReached);
                        CurrentOrders == [] ->
                            eventHandler(LocalOrders, GlobalOrders, sweepUp, FloorReached)
                    end;
                sweepDown ->
                    if
                        ApexFloorDown /= noApex->
                            process_fsm ! {targetFloor, ApexFloorDown},
                            orderManager:removeOrder(FloorReached, sweepDown),
                            eventHandler(LocalOrders, GlobalOrders, sweepDown, FloorReached);
                        ApexFloorUp /= noApex->
                            process_fsm ! {targetFloor, ApexFloorUp},
                            orderManager:removeOrder(FloorReached, sweepUp),
                            orderManager:removeOrder(FloorReached, sweepDown),
                            eventHandler(LocalOrders, GlobalOrders, sweepUp, FloorReached);
                        AreThereOrdersOnThisFloor ->
                            process_fsm ! {openDoors},
                            orderManager:removeOrder(FloorReached, sweepUp),
                            orderManager:removeOrder(FloorReached, sweepDown),
                            eventHandler(LocalOrders, GlobalOrders, sweepDown, FloorReached);
                        CurrentOrders == [] ->
                            eventHandler(LocalOrders, GlobalOrders, sweepDown, FloorReached)
                    end

            end
    end.





% returns noApex if there are no orders beyond currentfloor, given the current sweepstate
% returns a floor number if an apex floor exists. ex: getApexFloor([{order, 2, hall_up}], 0, sweepUp) -> 2
getApexFloor(Orders, CurrentFloor, SweepState) ->
    case SweepState of
        sweepUp ->
            OrdersAboveCurrentFloor = ordersAboveCurrentFloor(Orders, CurrentFloor),
            case OrdersAboveCurrentFloor of
                [] ->
                    noApex;
                _OrdersExist ->
                    [ApexOrder|_Rest] = lists:sort(fun(OrderA, OrderB) -> isOrderAOverOrderB(OrderA, OrderB) end, OrdersAboveCurrentFloor),
                    {order, ApexFloor, _OrderType} = ApexOrder,
                    ApexFloor
            end;
        sweepDown ->
            OrdersBelowCurrentFloor = ordersBelowCurrentFloor(Orders, CurrentFloor),
            case OrdersBelowCurrentFloor of
                [] ->
                    noApex;
                _OrdersExist ->
                    [ApexOrder|_Rest] = lists:sort(fun(OrderA, OrderB) -> isOrderAOverOrderB(OrderB, OrderA) end, OrdersBelowCurrentFloor),
                    {order, ApexFloor, _OrderType} = ApexOrder,
                    ApexFloor
            end
    end.


% returns true if there are orders on the floor Floor
areThereOrdersOnThisFloor(Orders, Floor) ->
    OrdersOnCurrentFloor = lists:filter(fun(Order) -> isOrderOnCurrentFloor(Order, Floor) end, Orders),
    case OrdersOnCurrentFloor of
        [] ->
            false;
        _AnyOrders ->
            true
    end.

% returns true if there are orders on the floor Floor going in the same direction as the elevator
shouldIStopOnThisFloor(Orders, SweepState, Floor) ->
    OrdersOnCurrentFloor = lists:filter(fun(Order) -> isOrderOnCurrentFloor(Order, Floor) end, Orders),
    OrdersInTheRightDirection = lists:filter(fun(Order) -> isOrderInTheRightDirection(Order, SweepState) end, OrdersOnCurrentFloor),
    case OrdersInTheRightDirection of
        [] ->
            false;
        _AnyOrders ->
            true
    end.




%------------ self explanatory help functions ---------------
ordersAboveCurrentFloor(Orders, CurrentFloor) ->
    lists:filter(fun(Order) -> isOrderAOverOrderB(Order, {order, CurrentFloor, none}) end, Orders).
ordersBelowCurrentFloor(Orders, CurrentFloor) ->
    lists:filter(fun(Order) -> isOrderAOverOrderB({order, CurrentFloor, none}, Order) end, Orders).

isOrderOnCurrentFloor({order, CurrentFloor, _}, CurrentFloor) ->
    true;
isOrderOnCurrentFloor({order, _, _}, _CurrentFloor) ->
    false.

isOrderInTheRightDirection({order, _Floor, cab}, _AnySweepState)->
    true;
isOrderInTheRightDirection({order, _Floor, OrderType}, SweepState)->
    case OrderType of
        hall_up ->
            SweepState == sweepUp;
        hall_down ->
            SweepState == sweepDown
    end.

isOrderAOverOrderB(OrderA, OrderB) ->
    {_order, FloorA, _typeA} = OrderA,
    {_order, FloorB, _typeB} = OrderB,
    FloorA > FloorB.

isOrderAOverFloorB(OrderA, FloorB) ->
    {_order, FloorA, _typeA} = OrderA,
    FloorA > FloorB.


testOrders() ->
  [{order,0,cab},{order,2,cab},{order,3,cab},{order,3,hall_down},{order,0,hall_up},{order,2,hall_up}].
