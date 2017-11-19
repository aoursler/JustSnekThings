% Just Sneck Things
%
% Writen by: Anne Oursler, November 2017
% Code Reviewed by:


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

% STUB FUNCTION DEFINITIONS
% handle_call({move, UserName, Direction}, _From, LoopData) ->
%    % TODO: Use erlport to send username and direction to snek game.
%    io:fwrite("~s would like to move ~s ~n", [UserName, Direction]),
%    {reply, LoopData}.

% move(HostName, GameName, UserName) ->
%    %TODO: Get arow key press or --quit
%    %      Without prompt & try to hide keypress as well?
%    Move = io:get_line("~w: ", [UserName]),
%    case Move of
%        "--quit\n" -> {ok};
%        Move ->
%            gen_server:call({GameName, HostName},
%                            {move, Move, {UserName, node()}}),
%            move(HostName, GameName, UserName)
%    end.

% receiveMessages() ->
%    receive
%        {{Sender, _Node}, Board} -> io:fwrite("~w: ~s", [Sender, Board]) %TODO: Make board 2D array, print 2D array
%        %TODO: Send board to erlport to display??
%    end,
%    receiveMessages().



% CLIENT GLOBAL CONSTANTS
boardWidth() -> 160.
boardHeight() -> 40.

% CLIENT FUNCTIONS
join_game(HostName, GameName, UserName) ->
    subscribe(HostName, GameName, UserName),
    % resizes the window to BoardHeight x BoardWidth
    io:fwrite("\e[8;~w;~wt", [boardHeight(), boardWidth()]),
    %Pid = spawn_link(?MODULE, receiveMessages, []),
    %io:fwrite("Pid: ~w~n", [Pid]),
    global:register_name(UserName, spawn_link(?MODULE, receiveMessages, [])), %Pid),
    io:fwrite("joined_game~n"),
    move(HostName, GameName, UserName),
    unsubscribe(HostName, GameName, UserName).

move(HostName, GameName, UserName) ->
    %TODO ANNE: Get arow key press or --quit
    %      Without prompt & try to hide keypress as well?
    Move = io:get_chars("", 1),
    io:fwrite("got a char~n"),
    case Move of
        "--quit\n" -> {ok};
        Move ->
            gen_server:call({GameName, HostName},
                            {move, Move, {UserName, node()}}),
            io:fwrite("moved"),
            move(HostName, GameName, UserName)
    end.

% This is esentually a mailbox that recieves the updated board and prints it to the screen repeatedly
% Since the screen is the exact size desired, this should completely replace the previous game state, and create a new one.
receiveMessages() ->
    io:fwrite("receiving_messages~n"),
    receive
        {{Sender, _Node}, Board} -> io:fwrite("~w: ~s", [Sender, Board])
        %TODO LEXI: Send board to erlport to display, or other display method.
    end,
    receiveMessages().
subscribe(HostName, GameName, UserName) ->
    gen_server:cast({GameName, HostName}, {subscribe, {UserName, node()}}).
unsubscribe(HostName, GameName, UserName) ->
    gen_server:cast({GameName, HostName}, {unsubscribe, {UserName, node()}}).

% SERVER FUNCTIONS
start_link(ChatName) ->
    {ok, Pid} = gen_server:start_link({global, ChatName}, ?MODULE, [],[]),
    io:fwrite("server_started~n"),
    register(ChatName, Pid),
    {ChatName, Pid}.
stop(ChatName) ->
    unregister(ChatName),
    gen_server:stop({global, ChatName}),
    io:fwrite("server_stopped~n").
init(LoopData) ->
    {ok, LoopData}.
handle_cast({subscribe, UserName}, LoopData) ->
    io:fwrite("subscribing~n"),
    {noreply, [UserName | LoopData]};
handle_cast({unsubscribe, UserName}, LoopData) ->
    io:fwrite("unsubscribing~n"),
    {noreply, filterOut(UserName, LoopData)}.
handle_call({move, UserName, Direction}, _From, LoopData) ->
    % TODO MATT: Use erlport to send username and direction to snek game.
    io:fwrite("~s would like to move ~s ~n", [UserName, Direction]),
    {reply, LoopData}.

terminate(Reason, _LoopData) ->
    io:fwrite("server_stopping~n"),
    exit(self(), Reason).
filterOut(_Element, []) -> [];
filterOut(Element, [Element | Tail]) -> filterOut(Element, Tail);
filterOut(Element, [Head | Tail]) -> [Head | filterOut(Element, Tail)].
