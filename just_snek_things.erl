% Just Sneck Things
%
% Writen by: Anne Oursler, November 2017
% Code Reviewed by:

%TODO: Remove print statements from this code. They are for debugging purposes
%      before we have a frontend

-module(just_snek_things).
-behavior(gen_server).

% CLIENT FUNCTION DEFINITIONS
-export([subscribe/3, unsubscribe/3]).
-export([join_game/3, move/3, receiveMessages/0]).
-export([boardWidth/0, boardHeight/0]).

% SERVER FUNCTION DEFINITIONS
-export([start_link/1, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([filterOut/2]).


% CLIENT GLOBAL CONSTANTS
boardWidth() -> 160.
boardHeight() -> 40.

% CLIENT FUNCTIONS
join_game(HostName, GameName, UserName) ->
    subscribe(HostName, GameName, UserName),
    % resizes the window to BoardHeight x BoardWidth
    io:fwrite("\e[8;~w;~wt", [boardHeight(), boardWidth()]),
    global:register_name(UserName, spawn_link(?MODULE, receiveMessages, [])),
    io:fwrite("joined_game~n"),
    move(HostName, GameName, UserName),
    unsubscribe(HostName, GameName, UserName).

move(HostName, GameName, UserName) ->
    Move = io:get_line(""),
    io:fwrite("got a char~n"),
    %TODO: Integrate with Lexi's frontend which will get moves char by char
    %      without pressing ENTER.
    case Move of
        "--quit\n" -> {ok};
        "w\n" ->
            gen_server:cast({GameName, HostName}, {move_up, UserName}), 
            io:fwrite("moved up~n"),
            move(HostName, GameName, UserName);
        "a\n" ->
            gen_server:cast({GameName, HostName}, {move_left, UserName}),                   
            io:fwrite("moved left~n"),
            move(HostName, GameName, UserName);
        "s\n" ->
            gen_server:cast({GameName, HostName}, {move_down, UserName}),                   
            io:fwrite("moved down~n"),
            move(HostName, GameName, UserName);
        "d\n" ->
            gen_server:cast({GameName, HostName}, {move_right, UserName}),                   
            io:fwrite("moved right~n"),
            move(HostName, GameName, UserName);
        Move ->
            io:fwrite("ERROR, BAD INPUT, NOTHING SENT TO SERVER~n"),
            move(HostName, GameName, UserName)
    end.

% This is esentually a mailbox that recieves the updated board and prints it
% to the screen repeatedly. Since the screen is the exact size desired, this
% should completely replace the previous game state, and create a new one.
receiveMessages() ->
    io:fwrite("receiving_messages~n"),
    receive
        {{Sender, _Node}, Board} -> io:fwrite("~w: ~s", [Sender, Board])
        %TODO: (lexi) Send board to erlport to display, or other display method.
    end,
    receiveMessages().
subscribe(HostName, GameName, UserName) ->
    gen_server:cast({GameName, HostName}, {subscribe, {UserName, node()}}).
unsubscribe(HostName, GameName, UserName) ->
    gen_server:cast({GameName, HostName}, {unsubscribe, {UserName, node()}}).


% SERVER FUNCTIONS
start_link(GameName) ->
    {ok, Pname} = python:start(),
    {ok, Pid} = gen_server:start_link({global, GameName}, ?MODULE, [Pname],[]),
    io:fwrite("server_started~n"),
    register(GameName, Pid),
    {GameName, Pid}.
stop(GameName) ->
    unregister(GameName),
    gen_server:stop({global, GameName}),
    io:fwrite("server_stopped~n").
init(Pname) ->
    {ok, {Pname, []}}.
handle_cast({subscribe, UserName}, {Pname, LoopData}) ->
    io:fwrite("subscribing~n"),
    {noreply, {Pname, [UserName | LoopData]}};
handle_cast({unsubscribe, UserName}, {Pname, LoopData}) ->
    io:fwrite("unsubscribing~n"),
    {noreply, {Pname, filterOut(UserName, LoopData)}};
handle_cast({"move_left", UserName}, {Pname, LoopData}) ->
    io:fwrite("moving left ~n"),
    python:call(Pname, snek, move, [UserName, "a"]),  
    {noreply, {Pname, LoopData}};
handle_cast({"move_right", UserName}, {Pname, LoopData}) ->
    io:fwrite("moving right ~n"),
    python:call(Pname, snek, move, [UserName, "d"]),  
    {noreply, {Pname, LoopData}};
handle_cast({"move_up", UserName}, {Pname, LoopData}) ->
    io:fwrite("moving up ~n"),
    python:call(Pname, snek, move, [UserName, "w"]),  
    {noreply, {Pname, LoopData}};
handle_cast({"move_down", UserName}, {Pname, LoopData}) ->
    io:fwrite("moving down ~n"),
    python:call(Pname, snek, move, [UserName, "s"]),
    {noreply, {Pname, LoopData}}.
handle_call(_Request, _From, State) ->
    {reply, State}.

terminate(Reason, _LoopData) ->
    io:fwrite("server_stopping~n"),
    exit(self(), Reason).
filterOut(_Element, []) -> [];
filterOut(Element, [Element | Tail]) -> filterOut(Element, Tail);
filterOut(Element, [Head | Tail]) -> [Head | filterOut(Element, Tail)].
