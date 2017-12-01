% Just Sneck Things
%
% Writen by: Anne Oursler, November 2017
% Code Reviewed by:

%TODO: Remove print statements from this code. They are for debugging purposes
%      before we have a frontend

-module(just_snek_things).
-behavior(gen_server).

% CLIENT FUNCTION DEFINITIONS
-export([subscribe/4, unsubscribe/4]).
-export([join_game/4, move/4, receiveMessages/0]).
-export([boardWidth/0, boardHeight/0]).

% SERVER FUNCTION DEFINITIONS
-export([start_link/1, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([filterOut/2]).


% CLIENT GLOBAL CONSTANTS
boardWidth() -> 160.
boardHeight() -> 40.

% CLIENT FUNCTIONS


% join_game( GameName, HostName, UserName, UserNode ): 
join_game( GameName, HostName, UserName, UserNode ) ->

    % subscribes to the given Game on the given Host with the given UserName
    subscribe( GameName, HostName, UserName, UserNode ),
    % resizes the window to BoardHeight x BoardWidth
    io:fwrite("\e[8;~w;~wt", [boardHeight(), boardWidth()]),
    % registers UserName to the PID of the server->client receive loop
    register( UserName, spawn_link( ?MODULE, receiveMessages, [] ) ),
    io:fwrite("joined_game~n"),
    % starts a move loop which will persist until client exit
    move( GameName, HostName, UserName, UserNode ),
    % once move loop ends, player is done: udid a full linking and some light commenting on your .erl file and my python file. python file's not brokenlnsubscribe
    unsubscribe( GameName, HostName, UserName, UserNode ).

% move( GameName, HostName, UserName, UserNode ): main loop to receive moves 
%   from python client to pass through to server
move( GameName, HostName, UserName, UserNode ) ->
    
    Move = io:get_line(""),
    io:fwrite("got a char~n"),
    %TODO: Integrate with Lexi's frontend which will get moves char by char
    %      without pressing ENTER.
    case Move of
        "--quit\n" -> { ok  };
        "w\n" ->
            gen_server:cast( { GameName, HostName }, { move_up, UserName, UserNode } ), 
            io:fwrite("moved up~n"),
            move( HostName, GameName, UserName, UserNode );
        "a\n" ->
            gen_server:cast( { GameName, HostName }, { move_left, UserName, UserNode } ),                   
            io:fwrite("moved left~n"),
            move( GameName, HostName, UserName, UserNode );
        "s\n" ->
            gen_server:cast( { GameName, HostName }, { move_down, UserName, UserNode } ),                   
            io:fwrite("moved down~n"),
            move( GameName, HostName, UserName, UserNode );
        "d\n" ->
            gen_server:cast( { GameName, HostName }, { move_right, UserName, UserNode } ),                   
            io:fwrite("moved right~n"),
            move( GameName, HostName, UserName, UserNode );
        Move ->
            io:fwrite("ERROR, BAD INPUT, NOTHING SENT TO SERVER~n"),
            move( GameName, HostName, UserName, UserNode )
    end.

% This is essentially a mailbox that receives the updated board and prints it
% to the screen repeatedly. Since the screen is the exact size desired, this
% should completely replace the previous game state, and create a new one.
% Ultimately this will be a junctionn to the client python board PID
receiveMessages() ->
    io:fwrite("receiving_messages~n"),
    receive
        %TODO: From Matt - this loop will need to maintain, at the very least, the python
        %   PID of Lexi's front end to send it the board
        { { Sender, _Node }, Board } -> io:fwrite("~w: ~s", [Sender, Board])
        %TODO: (lexi) Send board to erlport to display, or other display method.
    end,

    receiveMessages().

% subscribe( GameName, HostName, UserName, UserNode ):  asks to join GameName 
%   on node HostName, with UserName on noded UserNode
subscribe( GameName, HostName, UserName, UserNode ) ->
    gen_server:cast( { GameName, HostName }, { subscribe, 
        { UserName, UserNode } } ).

% unsubscribe( HostName, GameName, UserName, UserNode ): unsubscribes from 
%   GameName on node HostName, UserName/UserNode for unsub
unsubscribe( HostName, GameName, UserName, UserNode ) ->
    gen_server:cast( { GameName, HostName }, { unsubscribe, 
        { UserName, UserNode } } ).


% SERVER FUNCTIONS

% start_link( GameName ): starts a new game with input GameName. 
%   Python game VM is started and fields initialized. gen_server is started 
%   and GameName is registered to started server PID. Returns 
%   { ok, GameName, node() } 
start_link( GameName ) ->
    % Python VM started at PID Pname
    { ok, Pname } = python:start(),
    % Pname called to instantiate snek fields
    python:call( Pname, snek, make_fields, [GameName, node()] ),
    % Gameserver is started with gen_server link: passed Pname
    { ok, Pid } = gen_server:start_link( { local, GameName }, ?MODULE, [Pname],[] ),
    io:fwrite( "server_started~n" ),
    io:fwrite( "~w~n", [Pid]),
    % GameName registered to gameserver PID
    %register( GameName, Pid ),
    { ok, GameName, node(Pid) }.

% stop( { GameName, NodeName } ): ends the gameserver
stop( { GameName, NodeName } ) ->
    % bookkeeping to unregister GameName from gameserver PID
    unregister( GameName ),
    % stops gameserver
    gen_server:stop( { GameName, NodeName } ),
    io:fwrite("server_stopped~n").

% init( Pname ): internal function to construct state data for gameserver on
%   gen_server:start_link 
init( Pname ) ->
    { ok, { Pname, [] } }.

% handle_cast(subscribe): takes in UserName and UserNode for subscription,
%   adds player to python game, updates state 
handle_cast( { subscribe, UserName, UserNode } , { Pname, LoopData } ) ->
    io:fwrite("subscribing~n"),
    python:call( Pname, snek, add_player, [ UserName, UserNode ] ),
    { noreply, { Pname, [ { UserName,UserNode } | LoopData ] } };

% handle_cast(unsubscribe): takes in UserName and UserNode for unsubscription,
%   removes player from python game, updates state
handle_cast( { unsubscribe, UserName, UserNode }, { Pname, LoopData } ) ->
    io:fwrite("unsubscribing~n"),
    python:call( Pname, snek, remove_player, [ UserName, UserNode ] ),
    { noreply, { Pname, filterOut( { UserName, UserNode }, LoopData ) } };

% handle_cast(moves): sends UserName, UserNode and move to python game
%  TODO: from/for Matt: python call returns failure tuple on death. integrate.
handle_cast( { "move_left", UserName, UserNode }, { Pname, LoopData } ) ->
    io:fwrite("moving left ~n"),
    python:call( Pname, snek, move, [UserName, UserNode, "a"] ),  
    { noreply, { Pname, LoopData } };
handle_cast( { "move_right", UserName, UserNode }, { Pname, LoopData } ) ->
    io:fwrite("moving right ~n"),
    python:call( Pname, snek, move, [UserName, UserNode, "d"] ),  
    { noreply, { Pname, LoopData } };
handle_cast( { "move_up", UserName, UserNode }, { Pname, LoopData } ) ->
    io:fwrite("moving up ~n"),
    python:call( Pname, snek, move, [UserName, UserNode, "w"] ),  
    { noreply, { Pname, LoopData } };
handle_cast( { "move_down", UserName, UserNode }, { Pname, LoopData } ) ->
    io:fwrite("moving down ~n"),
    python:call( Pname, snek, move, [UserName, UserNode, "s"] ),
    { noreply, { Pname, LoopData } }.

% handle_call/3: stub for handle_call
handle_call( _Request, _From, State ) ->
    { reply, State }.

% terminate/2 is an internal function called by the gen_server's stop()
%   upon exiting to terminate all connected chat client processes.
terminate( _Reason, _LoopData ) ->
    io:fwrite("server_stopping~n"),
    exit( self(), _Reason ).

% filterOut( Element, List) : tail-recursive function removes all instances of
%   input Element from input List
filterOut( _Element, []) -> [];
filterOut( Element, List ) -> filterOut( Element, List, [] ).

% internal filterOut/3 for filterOut/2 tail recursion
filterOut( _Element, [], Keep ) -> Keep;
filterOut( Element, [Element | Tail], Keep ) -> 
    filterOut( Element, Tail, Keep );
filterOut( Element, [Head | Tail], Keep ) -> 
    filterOut( Element, Tail, [Keep|Head] ).
