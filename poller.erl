-module(poller).
-compile(export_all).

-define(NUM_FLOORS, 4).
-define(POLL_DELAY, 100).

-define(CabButtons, lists:seq(0,?NUM_FLOORS-1)).
-define(HallDownButtons, lists:seq(1,?NUM_FLOORS-1)).
-define(HallUpButtons, lists:seq(0, ?NUM_FLOORS-2)).


pollButton(FloorNumber, ButtonType, 0) ->
  timer:sleep(?POLL_DELAY),
  ButtonState = driver:get_order_button_state(process_driver, FloorNumber, ButtonType),
  case ButtonState of
    1 ->
      io:format("[POLLER] -> button press recieved ~p ~p ~n", [ButtonType, FloorNumber]),
      orderManager:addOrder(FloorNumber,ButtonType),
      pollButton(FloorNumber, ButtonType, 1);
    0 ->
      pollButton(FloorNumber, ButtonType, 0)
  end;

pollButton(FloorNumber, ButtonType, 1) ->
  timer:sleep(?POLL_DELAY),
  ButtonState = driver:get_order_button_state(process_driver, FloorNumber, ButtonType),
  pollButton(FloorNumber, ButtonType, ButtonState).



pollFloorSensor(between_floors)->
  timer:sleep(?POLL_DELAY),
  FloorState = driver:get_floor_sensor_state(process_driver),
  pollFloorSensor(FloorState);


pollFloorSensor(idle) ->
  timer:sleep(?POLL_DELAY),
  FloorState = driver:get_floor_sensor_state(process_driver),
  case FloorState of
    between_floors ->
      pollFloorSensor(between_floors);
    _ ->
      pollFloorSensor(idle)
  end;


pollFloorSensor(Floor) ->
  process_fsm ! {reachedFloor, Floor},
  pollFloorSensor(idle).


start() ->
  lists:foreach(fun(Floor) -> spawn(fun() -> pollButton(Floor, cab, 0) end) end, ?CabButtons),
  lists:foreach(fun(Floor) -> spawn(fun() -> pollButton(Floor, hall_up, 0) end) end, ?HallUpButtons),
  lists:foreach(fun(Floor) -> spawn(fun() -> pollButton(Floor, hall_down, 0) end) end, ?HallDownButtons),
  spawn(fun() -> pollFloorSensor(between_floors) end),
  io:format("[POLLER] -> poller initialized~n").
