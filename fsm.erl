-module(fsm).
-compile(export_all).

-define(RECEIVE_BLOCK_TIME, 2000).
-define(DOOR_OPEN_TIME, 2500).

start() -> % go down until the elevator reaches the ground floor and then tell the Event_Handler(or Main) that initialization is finished
	initLights(process_driver),
	driver:set_motor_direction(process_driver, down),
	receive
		{reachedFloor, FloorNumber} ->
			case FloorNumber of
				0 ->
					driver:set_motor_direction(process_driver,stop),
					driver:set_floor_indicator(process_driver,0),
					process_eventHandler ! {reachedFloor, 0},
					io:format("[STATE MACHINE] -> fsm initialized ~n"),
					state_idle(0);
				_ ->
					start()
			end

	end.


state_idle(FloorNumber) ->
	%io:format("~n[STATE MACHINE] IDLE STATE ~n"),
	receive
		{targetFloor, TargetFloor} ->
		  io:format("[STATE MACHINE] -> New target ~p ~n", [TargetFloor]),
		  if
		  	FloorNumber<TargetFloor ->
		  		driver:set_motor_direction(process_driver, up),
		  		state_driving(TargetFloor);
		  	FloorNumber>TargetFloor ->
		  		driver:set_motor_direction(process_driver, down),
		  		state_driving(TargetFloor);
		  	FloorNumber==TargetFloor ->
		  		%io:format("[STATE MACHINE] received floor_reached, stopping, opening doors~n"),
		  		driver:set_motor_direction(process_driver, stop),
		  		state_doors_open(FloorNumber)
			end;
		{openDoors} ->
			state_doors_open(FloorNumber)
	after
			1000 ->
				process_eventHandler ! {reachedTargetFloor, FloorNumber},
				state_idle(FloorNumber)
	end.



state_driving(TargetFloor) ->
 	%io:format("[STATE MACHINE] DRIVING STATE ~n"),
  	receive
    	{reachedFloor, FloorNumber} ->
			driver:set_floor_indicator(process_driver, FloorNumber),
      		if
				FloorNumber==TargetFloor ->
					%io:format("[STATE MACHINE] reached target floor ~p~n", [FloorNumber]),
					process_eventHandler ! {reachedTargetFloor, FloorNumber},
					driver:set_motor_direction(process_driver, stop),
					state_doors_open(FloorNumber);
				FloorNumber<TargetFloor ->
					%io:format("[STATE MACHINE] reached floor ~p~n", [FloorNumber]),
					driver:set_motor_direction(process_driver, up),
					process_eventHandler ! {reachedFloor, FloorNumber},
					state_driving(TargetFloor);
				FloorNumber>TargetFloor ->
					%io:format("[STATE MACHINE] reached floor ~p~n", [FloorNumber]),
					driver:set_motor_direction(process_driver, down),
					process_eventHandler ! {reachedFloor, FloorNumber},
					state_driving(TargetFloor)
			end;
		{stopOnThisFloor, FloorNumber} ->
			io:format("[STATE MACHINE] stopping on this floor ~p~n", [FloorNumber]),
			driver:set_motor_direction(process_driver, stop),
			temporary_stop(FloorNumber, TargetFloor)
  end.



state_doors_open(FloorNumber) ->
	%io:format("[STATE MACHINE] OPENING DOORS STATE~n"),
	%io:format("[STATE MACHINE] opening doors~n"),
	driver:set_door_open_light(process_driver,on),
	timer:sleep(?DOOR_OPEN_TIME),
	%io:format("[STATE MACHINE] closing doors~n"),
	driver:set_door_open_light(process_driver,off),
  	state_idle(FloorNumber).


%This state is only for stops which arent targetFloor. This prevents an elevator from passing an order
temporary_stop(FloorNumber, TargetFloor) ->
	%io:format("[STATE MACHINE] OPENING DOORS STATE~n"),
	%io:format("[STATE MACHINE] opening doors~n"),
	driver:set_door_open_light(process_driver,on),
	timer:sleep(?DOOR_OPEN_TIME),
	%io:format("[STATE MACHINE] closing doors~n"),
	driver:set_door_open_light(process_driver,off),
	if
	  FloorNumber<TargetFloor ->
		  driver:set_motor_direction(process_driver, up),
		  state_driving(TargetFloor);
	  FloorNumber>TargetFloor ->
		  driver:set_motor_direction(process_driver, down),
		  state_driving(TargetFloor)
	end.


initLights(Pid) ->
    driver:set_floor_indicator(Pid, 0),
    driver:set_door_open_light(Pid, off),
    driver:set_order_button_light(Pid, hall_up, 0, off),
    driver:set_order_button_light(Pid, hall_up, 1, off),
    driver:set_order_button_light(Pid, hall_up, 2, off),
    driver:set_order_button_light(Pid, hall_down, 1, off),
    driver:set_order_button_light(Pid, hall_down, 2, off),
    driver:set_order_button_light(Pid, hall_down, 3, off),
    driver:set_order_button_light(Pid, cab, 0, off),
    driver:set_order_button_light(Pid, cab, 1, off),
    driver:set_order_button_light(Pid, cab, 2, off),
    driver:set_order_button_light(Pid, cab, 3, off).
