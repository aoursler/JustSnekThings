% Just Sneck Things
%
% Writen by: Anne Oursler, October 2017
% Code Reviewed by:

-module(just_snek_things).
-export([start_link/1, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([subscribe/3, unsubscribe/3]).
-export([join_game/3, move/0]).
-behavior(gen_server).

% GLOBAL CONSTANTS
BoardWidth = 160
BoardHeight = 40

% SERVER FUNCTIONS
start_link(ChatName) ->
    {ok, Pid} = gen_server:start_link({global, ChatName}, ?MODULE, [],[]),
    register(ChatName, Pid),
    {ChatName, Pid}.
stop(ChatName) ->
    unregister(ChatName),
    gen_server:stop({global, ChatName}).
init(LoopData) ->
    {ok, LoopData}.
handle_cast({subscribe, UserName}, LoopData) ->
    {noreply, [UserName | LoopData]};
handle_cast({unsubscribe, UserName}, LoopData) ->
    {noreply, filterOut(UserName, LoopData)};
% TODO: Handle cast/call for move
%       Use erlport to run snek, 

terminate(Reason, _LoopData) ->
    exit(self(), Reason).
filterOut(_Element, []) -> [];
filterOut(Element, [Element | Tail]) -> filterOut(Element, Tail);
filterOut(Element, [Head | Tail]) -> [Head | filterOut(Element, Tail)].

% CLIENT FUNCTIONS
join_game(HostName, GameName, UserName) ->
    subscribe(HostName, GameName, UserName),
    # resizes the window to BoardHeight x BoardWidth
    io:fwrite("\e[8;~w;~wt", [BoardHeight, BoardWidth]),
    Pid = spawn_link(?MODULE, receiveMessages, []),
    register(UserName, Pid),
    move(HostName, GameName, UserName),
    unsubscribe(HostName, GameName, UserName).

move(HostName, GameName, UserName) ->
    #TODO: Get arow key press or --quit 
    #      Without prompt & try to hide keypress as well?
    Move = io:get_line("~w: ", [UserName]),
    case Move of
        "--quit\n" -> {ok};
        Move ->
            gen_server:call({GameName, HostName},
                            {move, Move, {UserName, node()}}),
            move(HostName, GameName, UserName)
    end.

% This is esentually a mailbox that recieves the updated board and prints it to the screen repeatedly
% Since the screen is the exact size desired, this should completely replace the previous game state, and create a new one.
receiveMessages() ->
    receive
        {{Sender, Node}, Board} -> io:fwrite("~w: ~s", [Sender, Message]) %TODO: Make board 2D array, print 2D array
        %TODO: Send board to erlport to display??
    end,
    receiveMessages().
subscribe(HostName, GameName, UserName) ->
    gen_server:cast({GameName, NodeName}, {subscribe, {UserName, node()}}).
unsubscribe(HostName, GameName, UserName) ->
    gen_server:cast({GameName, HostName}, {unsubscribe, {UserName, node()}}).
