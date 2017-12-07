% Just Sneck Things

-module(just_snek_things).
-behavior(gen_server).

% CLIENT FUNCTION DEFINITIONS
-export([subscribe/4, unsubscribe/4]).
-export([join_game/4, move/6 ]). %
-export([boardWidth/0, boardHeight/0]).

% SERVER FUNCTION DEFINITIONS
-export([start_link/1, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([filterOut/2, timer/1, get_board/1]).


% CLIENT GLOBAL CONSTANTS
boardWidth() -> 50. %160.
boardHeight() -> 50. %40.

% CLIENT FUNCTIONS

% join_game( GameName, HostName, UserName, UserNode ): Client function that joins
%   GameName on node NodeName with UserName on cleint UserNode.
join_game( ServerName, ServerNode, UserName, UserNode ) ->
    {ok, Pfront} = python:start([{python_path, "/"}]),
    % subscribes to the given Game on the given Host with the given UserName
    subscribe( ServerName, ServerNode, UserName, UserNode ),
    % spawns the client loop that responds to move and persists until the cilent exits
    ServerPid = spawn_link(node(), ?MODULE, move, [ServerName, ServerNode, UserName, UserNode, Pfront, {[]}] ),
    % registers UserName to the PID of the server->client receive loop
    register( UserName, ServerPid),
    % starts the frontend
    python:call(Pfront, frontend, snekGUI, [boardWidth(), boardHeight(), ServerPid]).


% move( GameName, HostName, UserName, UserNode ): main loop to receive moves
%   from python client to pass through to server
move( ServerName, ServerNode, UserName, UserNode, Pfront, Board) ->
    receive
	% link: tell the client to link to the frontend
        link -> link(Pfront),
                move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );    
	% board: update the board with a new board from the backend
        { board, NewBoard } ->
            move( ServerName, ServerNode, UserName, UserNode, Pfront, NewBoard );
	% getBoard: send the current board back to the sender (frontend)
        { getBoard, Sender } ->
            Sender ! Board,
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board);
	% up: tell the server to move the client's snek up
        up ->
            gen_server:cast( { ServerName, ServerNode },
                { move_up, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );
        % left: tell the server to move the client's snek left
        left ->
            gen_server:cast( { ServerName, ServerNode },
                { move_left, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );
        % down: tell the server to move the client's snek down
        down ->
            gen_server:cast( { ServerName, ServerNode },
                { move_down, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );
        % right: tell the server to move the client's snek right
        right ->
            gen_server:cast( { ServerName, ServerNode },
                { move_right, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );
        % quit: tells the server to remove the user from the game
        quit ->
            gen_server:cast( { ServerName, ServerNode },
                { quit, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );    
	% wildcard and error message to catch bad messages sent to the client
        _Move ->
            io:fwrite("ERROR, BAD INPUT, NOTHING SENT TO SERVER~n")
    end.

% getboard( MovePid): asks for the board from the client
%    tby making a call to the move function. It is the interface between the 
%    erlang server and the python frontend.
get_board(MovePid) ->
    MovePid ! { getBoard, self() },
    receive
        Board -> Board
    end.

% subscribe( GameName, HostName, UserName, UserNode ):  asks to join GameName
%   on node HostName, with UserName on noded UserNode
subscribe( ServerName, ServerNode, UserName, UserNode ) ->
    gen_server:cast( { ServerName, ServerNode },
        { subscribe, { UserName, UserNode } } ).

% unsubscribe( HostName, GameName, UserName, UserNode ): unsubscribes from
%   GameName on node HostName, UserName/UserNode for unsub
unsubscribe( ServerName, ServerNode, UserName, UserNode ) ->
    gen_server:cast( { ServerName, ServerNode },
        { unsubscribe, { UserName, UserNode } } ).



% SERVER FUNCTIONS

% start_link( ServerName ): starts a new game with input ServerName.
%   Python game VM is started and fields initialized. gen_server is started
%   and ServerName is registered to started server PID. Returns
%   { ok, ServerName, node() }
start_link( ServerName ) ->
    % Python VM started at PID Pname
    { ok, Pname } = python:start(),
    % Pname called to instantiate snek fields
    python:call( Pname, snek, make_fields, [ServerName, node()] ),
    % Game server is started with gen_server link: passed Pname
    { ok, Pid } = gen_server:start_link( { local, ServerName },
        ?MODULE, [Pname], [] ),
    spawn_link(?MODULE, timer, [{ServerName, node(Pid)}]),
    % GameName registered to gameserver PID
    { ok, ServerName, node(Pid) }.

% stop( { GameName, NodeName } ): ends the gameserver
stop( { ServerName, ServerNode } ) ->
    % bookkeeping to unregister GameName from gameserver PID
    unregister( ServerName ),
    % stops gameserver
    gen_server:stop( { ServerName, ServerNode } ),
    io:fwrite("server_stopped~n").

% init( Pname ): internal function to construct state data for gameserver on
%   gen_server:start_link
init( [Pname] ) ->
    link(Pname),
    { ok, { [Pname], [] } }.

% handle_cast(subscribe): takes in UserName and UserNode for subscription,
%   adds player to python game, updates state
handle_cast( { subscribe, {UserName, UserNode }} , { [Pname], LoopData } ) ->
    _Response = python:call( Pname, snek, add_player, [ UserName, UserNode ] ),
    { noreply, { [Pname], [ { UserName,UserNode } | LoopData ] } };

% handle_cast(unsubscribe): takes in UserName and UserNode for unsubscription,
%    removes player from python game, updates state
handle_cast( { unsubscribe, {UserName, UserNode } }, { [Pname], LoopData } ) ->
    {Reply, {Uname,Unode}} = python:call( Pname, snek, remove_player, [ UserName, UserNode ] ),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], filterOut( { UserName, UserNode }, LoopData ) } };

% handle_cast quit: sends UserName, UserNode and quit to python game so that the player 
%    is removed from the game, then shuts down the cleint's front end
handle_cast( { quit, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, quit] ),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

% Moves: sends UserName, UserNode and move to python game
%    if the client is dead in the server, it shuts down the client
handle_cast( { move_left, { UserName, UserNode } }, { [Pname], LoopData } ) ->    
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, a] ),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

handle_cast( { move_right, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, d] ),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

handle_cast( { move_up, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, w] ),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

handle_cast( { move_down, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, s] ),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

% handle_cast update_board: takes in UserName and UserNode and triggers,
%    a call to snek for the board and send the board to all of the users
handle_cast({update_board }, { [Pname], LoopData } ) ->
    Board = python:call(Pname, snek, get_board, []),
    send_board(LoopData, Board),
    {noreply, { [Pname], LoopData}}.

% handle_call/3: stub for handle_call
handle_call( _Request, _From, State ) ->
    { reply, State }.

% terminate/2 is an internal function called by the gen_server's stop()
%   upon exiting to terminate all connected chat client processes.
terminate( kill, _LoopData ) ->
    exit( self(), kill ).

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

% repeatedly sends a message to the server to get a new board from the backend, then sends that board to all of the players 
timer({ServerName, ServerNode}) ->
    gen_server:cast({ServerName, ServerNode}, {update_board}),
    timer:sleep(250),
    timer({ServerName, ServerNode}).

% send the given board to all users in the list of users.
send_board([], _Board) ->
    {ok};
send_board([{UserName, UserNode}], Board) ->
    {UserName, UserNode} ! {board, Board};
send_board([{UserName, UserNode} | Usernames], Board) ->
    {UserName, UserNode} ! {board, Board},
    send_board(Usernames, Board).
