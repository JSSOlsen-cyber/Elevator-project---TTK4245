# How to run

> To start the elevator, run the bash script "./elevator_boot.sh" with the elevator server already running

# Module description

## watchdog.erl

> The watchDog monitors all the orders accepted by elevators across the entire elevator system. If an order is sent to an elevator and not handled within a reasonable time space, the watchdog will time out the order and send it to the order manager to be redistributed to a functioning elevator

## eventHandler.erl

> The eventHandler accepts a list of orders from the orderManager in real-time. It applies the SCAN algorithm to find a semi-optimal path between the orders allocated to the elevator. When requested by the FSM, the eventHandler supplies the FSM with a new target floor. The eventHandler also informs the watchDog when an order has been executed.

## poller.erl

> The poller continously polls the driver module for new information and sends the relevant information to the FSM and orderManager module

## orderManager.erl

> The order manager accepts incoming orders, either from other elevators or from the poller. The order manager keeps track of what orders should be serviced by the local elevator, and distributes orders to other elevators, when it is considered more efficient. The order manager also backs up local cab orders to storage, in case of an unexpected shutdown. 

## fsm.erl

> The FSM drives the elevator according to a received order (TargetFloor). When the floor is reached and the doors open, a message is sent to the eventHandler to inform him of a completed order.
It uses the given driver.erl file to move the elevator up and down, turn on/off the lights for instance.

## main.erl

> The main is the starting point. It compiles all the needed files and run the threads to operate the elevator. The elevator can be run on both UNIX based system and Windows based systems.

## elenet.erl

> The elenet module manages connections between Elevators. All Elevators running on the same network will automatically connect and syncronize. The elenet broadcasts a Beacon signal which allows Elevators on the same network to loacte each other and connect. If an elevator remains unresponsive for some time, it will locate the elenet Beacon signal which is broadcast on the system and reconnect to the existing elevators.

