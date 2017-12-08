%
% Just Snek Things
% Authors: Anne Oursler, Lexi Galantino, Matt Turner
%

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

% join_game: Function that joins the client to the game.
%
% Input:     ServerName: Name of the server running the game.
%            ServerNode: Name of the node the server is running on.
%            UserName:   Name the player want's to join with.
%            UserNode:   The node the user is running on. 
%                        It is recommened to use the node() function.
% Output:    Starts a Tkinter window on which to play Snek.
join_game( ServerName, ServerNode, UserName, UserNode ) ->
    {ok, Pfront} = python:start([{python_path, "/"}]),
    % subscribe to the given User to the given Server
    subscribe( ServerName, ServerNode, UserName, UserNode ),
    % spawns the client loop that responds to move
    % persists until the cilent exits
    ServerPid = spawn_link(node(), ?MODULE, move, 
	[ServerName, ServerNode, UserName, UserNode, Pfront, {[]}] ),
    % registers UserName to the PID of the server->client receive loop
    register( UserName, ServerPid),
    % starts the frontend
    python:call(Pfront, frontend, snekGUI,
	[boardWidth(), boardHeight(), ServerPid]).


% move: Function which loops to interface with the frontend for the
%       server. 
%
% Input:  ServerName: Name of the server running the game.
%         ServerNode: Name of the node the server is running on.
%         UserName:   Name the player.
%         UserNode:   The node the player is running on. 
%         Pfront:     The address of the python instance running the
%                     frontend.
%         Board:      The current state of the Snek board.
% Output: None, loops on completion.
move( ServerName, ServerNode, UserName, UserNode, Pfront, Board) ->
    receive
	% link: tell the client to link to the frontend
        link -> link(Pfront),
            move( ServerName, ServerNode, UserName, UserNode, 
		Pfront, Board );    
	% board: update the board with a new board from the backend
        { board, NewBoard } ->
            move( ServerName, ServerNode, UserName, UserNode,
		Pfront, NewBoard );
	% getBoard: send the current board back to the frontend
        { getBoard, Sender } ->
            Sender ! Board,
            move( ServerName, ServerNode, UserName, UserNode,
		Pfront, Board);
	% up: tell the server to move the client's snek up
        up ->
            gen_server:cast( { ServerName, ServerNode },
                { move_up, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode,
		Pfront, Board );
        % left: tell the server to move the client's snek left
        left ->
            gen_server:cast( { ServerName, ServerNode },
                { move_left, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode,
		Pfront, Board );
        % down: tell the server to move the client's snek down
        down ->
            gen_server:cast( { ServerName, ServerNode },
                { move_down, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode,
		Pfront, Board );
        % right: tell the server to move the client's snek right
        right ->
            gen_server:cast( { ServerName, ServerNode },
                { move_right, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode,
		Pfront, Board );
        % quit: tells the server to remove the user from the game
        quit ->
            gen_server:cast( { ServerName, ServerNode },
                { quit, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode,
		Pfront, Board );    
	% wildcard and error message to catch bad messages
        _Move ->
            io:fwrite("ERROR, BAD INPUT, NOTHING SENT TO SERVER~n")
    end.

% getboard: Asks move for the board and gives it to the frontend.
% 
% Input:    MovePid: The Pid of the move function relateing to the
%                    calling frontend.
% Output:   Board: the current state of the board.
get_board(MovePid) ->
    MovePid ! { getBoard, self() },
    receive
        Board -> Board
    end.

% subscribe: Sends a cast to the server to subscribe the user to
%            the game. 
%
% Input:  ServerName: Name of the server running the game.
%         ServerNode: Name of the node the server is running on.
%         UserName:   Name the player.
%         UserNode:   The node the player is running on. 
% Output: None.
subscribe( ServerName, ServerNode, UserName, UserNode ) ->
    gen_server:cast( { ServerName, ServerNode },
        { subscribe, { UserName, UserNode } } ).

% unsubscribe: Sends a cast to the server to unsubscribe the user from
%              the game.
%
% Input:  ServerName: Name of the server running the game.
%         ServerNode: Name of the node the server is running on.
%         UserName:   Name the player.
%         UserNode:   The node the player is running on. 
% Output: None.
unsubscribe( ServerName, ServerNode, UserName, UserNode ) ->
    gen_server:cast( { ServerName, ServerNode },
        { unsubscribe, { UserName, UserNode } } ).



% SERVER FUNCTIONS

% start_link: Starts a server for a new game of Snek. 
%
% Input:  ServerName: Name to give the server running the game.
% Output: {ok, ServerName, ServerNode} Where ServerNode is the node 
%         the game is running on.
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

% stop: Stops the Server ending the game of Snek.
%
% Input: ServerName: Name of the server running the game.
%        ServerNode: Name of the node the server is running on.
% Output: None.
stop( { ServerName, ServerNode } ) ->
    % bookkeeping to unregister GameName from gameserver PID
    unregister( ServerName ),
    % stops gameserver
    gen_server:stop( { ServerName, ServerNode } ).

% init: Internal function to construct state data for the gameserver.
% 
% Input: Pid of the python vm running the snek game in a list.
% Output: {ok, { [Pname], [] }}, where the empty list will be used to
%         hold usernames.
init( [Pname] ) ->
    link(Pname),
    { ok, { [Pname], [] } }.

% handle_cast(subscribe): Tells the backend that the player wants to
% 			  join, and adds the player to the server's 
% 			  list of players.
%
% Input:  {subscrive, {UserName, UserNode}}
%         UserName: Name the player.
%         UserNode: The node the player is running on.
% Output: {noreply, {[Pname], [{UserName, UserNode} | LoopData]}
%         Pname:    The python server where the backend of the game is
%         	    running.
%         LoopData: The list of UserName, UserNode tuples that are 
%                   in the game.
handle_cast( { subscribe, {UserName, UserNode }} , 
	{ [Pname], LoopData } ) ->
    _Response = python:call( Pname, snek, add_player, 
        [ UserName, UserNode ] ),
    { noreply, { [Pname], [ { UserName,UserNode } | LoopData ] } };

% handle_cast(unsubscribe): Tells the backend that the player wants to
% 			    leave, and removes the player from the
% 			    server's list of players.
%
% Input:  {unsubscribe, {UserName, UserNode}}:
%         UserName: Name the player.
%         UserNode: The node the player is running on.
%         {[Pname], LoopData}:
%         Pname:    The python server where the backend of the game is
%                   running.
%         LoopData: The list of UserName, UserNode tuples that are
%                   in the game.
% Output: {noreply, {[Pname], LoopData}}
%         Pname:    The python server where the backend of the game is
%         	    running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game without the UserName, UserNode tupple.
handle_cast( { unsubscribe, {UserName, UserNode } }, 
        { [Pname], LoopData } ) ->
    {Reply, {_Uname, _Unode} } = 
	python:call( Pname, snek, remove_player, 
		[ UserName, UserNode ] ),
    case Reply of 
        removed -> send_board( [{ UserName, UserNode }], die);
        serverQuit -> send_board( [{ UserName, UserNode }], die),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], 
	filterOut( { UserName, UserNode }, LoopData ) } };

% handle_cast(quit): Tells the backend that the player wants to leave,
%  		     and removes the player from the server's list of
%  		     players.
%
% Input:  {quit, {UserName, UserNode}}:
%         UserName: Name the player.
%         UserNode: The node the player is running on.
%         {[Pname], LoopData}:
%         Pname:    The python server where the backend of the game
%                   is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game.
% Output: {noreply, {[Pname], LoopData}}
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game without the UserName, UserNode tupple.
handle_cast( { quit, { UserName, UserNode } },
	{ [Pname], LoopData } ) ->
    {Reply, {_Uname, _Unode}} = python:call( Pname, snek, move,
	[UserName, UserNode, quit] ),
    case Reply of 
        removed -> send_board( [{ UserName, UserNode }], die);
        serverQuit -> send_board( [{ UserName, UserNode }], die),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

% MOVES

% handle_cast(move_left): Tells the backend that the player wants to
% 			  move left.
%
% Input:  {move_left, {UserName, UserNode}}:
%         UserName: Name the player.
%         UserNode: The node the player is running on.
%         {[Pname], LoopData}:
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game.
% Output: {noreply, {[Pname], LoopData}}
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game without the UserName, UserNode tupple.
handle_cast( { move_left, { UserName, UserNode } },
	{ [Pname], LoopData } ) ->    
    {Reply, {_Uname, _Unode}} = python:call( Pname, snek, move,
	[UserName, UserNode, a] ),
    % If the client is dead in the server, it shuts down the client.
    case Reply of 
        removed -> send_board( [{ UserName, UserNode }], die);
        serverQuit -> send_board( [{ UserName, UserNode }], die),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

% handle_cast(move_right): Tells the backend that the player wants to
% 			   movevright.
%
% Input:  {move_right, {UserName, UserNode}}:
%         UserName: Name the player.
%         UserNode: The node the player is running on.
%         {[Pname], LoopData}:
%         Pname:    The python server where the backend of the game
%                   is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game.
% Output: {noreply, {[Pname], LoopData}}
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game without the UserName, UserNode tupple.
handle_cast( { move_right, { UserName, UserNode } }, 
	{ [Pname], LoopData } ) ->
    {Reply, {_Uname, _Unode}} = python:call( Pname, snek, move,
        [UserName, UserNode, d] ),
    % If the client is dead in the server, it shuts down the client.
    case Reply of 
        removed -> send_board( [{ UserName, UserNode }], die);
        serverQuit -> send_board( [{ UserName, UserNode }], die),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

% handle_cast(move_up): Tells the backend that the player wants to
% 			move up.
%
% Input:  {move_up, {UserName, UserNode}}:
%         UserName: Name the player.
%         UserNode: The node the player is running on.
%         {[Pname], LoopData}:
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are
%         	    in the game.
% Output: {noreply, {[Pname], LoopData}}
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game without the UserName, UserNode tupple.
handle_cast( { move_up, { UserName, UserNode } },
	{ [Pname], LoopData } ) ->
    {Reply, {_Uname, _Unode}} = python:call( Pname, snek, move,
	[UserName, UserNode, w] ),
    % If the client is dead in the server, it shuts down the client.
    case Reply of 
        removed -> send_board( [{ UserName, UserNode }], die);
        serverQuit -> send_board([ { UserName, UserNode }], die),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

% handle_cast(move_down): Tells the backend that the player wants to
% 			  move down.
%
% Input:  {move_down, {UserName, UserNode}}:
%         UserName: Name the player.
%         UserNode: The node the player is running on.
%         {[Pname], LoopData}:
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game.
% Output: {noreply, {[Pname], LoopData}}
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game without the UserName, UserNode tupple.
handle_cast( { move_down, { UserName, UserNode } },
	{ [Pname], LoopData } ) ->
    {Reply, {_Uname, _Unode}} = python:call( Pname, snek, move,
	[UserName, UserNode, s] ),
    % If the client is dead in the server, it shuts down the client.
    case Reply of 
        removed -> send_board( [{ UserName, UserNode }], die);
        serverQuit -> send_board( [{ UserName, UserNode }], die),
                        exit( self(), kill);
        _reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

% handle_cast(update_board): Asks the backend for an updated board
% 			     and gives it to send_board.
%
% Input:  {update_board}
%         {[Pname], LoopData}
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game.
% Output: {noreply, {[Pname], LoopData}}
%         Pname:    The python server where the backend of the game
%         	    is running.
%         LoopData: The list of UserName, UserNode tuples that are in
%         	    the game without the UserName, UserNode tupple.
handle_cast({ update_board }, { [Pname], LoopData } ) ->
    Board = python:call(Pname, snek, get_board, []),
    send_board(LoopData, Board),
    {noreply, { [Pname], LoopData}}.

% handle_call: A stub to satify gen_server
%
% Input:  Request: The purpose of the call.
%         From: The sender of the call.
%         State: The data for the call.
% Output: {reply, State}
%         State: An updated set of data for the call.
handle_call( _Request, _From, State ) ->
    { reply, State }.

% terminate: The final exit function called by gen_server.
%
% Input:  kill: an atom telling terminate to kill the process.
%         LoopData: The list of usernames that the server had.
% Output: None. This function will kill the running process.
terminate( kill, _LoopData ) ->
    exit( self(), kill ).

% filterOut: Filters an element out of a given list, using tail 
% 	     recursion.
% 
% Input: Element: Element to filter out of the list.
%        List: The list to filter the element out of.
% Output: The list without the element.
filterOut( _Element, []) -> [];
filterOut( Element, List ) -> filterOut( Element, List, [] ).

% filterOut: Filters an element out of a given list, using tail
% 	     recursion.
% 
% Input: Element: Element to filter out of the list.
%        List: The list to filter the element out of.
%        Keep: The list without the element so far.
% Output: The list without the element.
filterOut( _Element, [], Keep ) -> Keep;
filterOut( Element, [Element | Tail], Keep ) ->
    filterOut( Element, Tail, Keep );
filterOut( Element, [Head | Tail], Keep ) ->
    filterOut( Element, Tail, [Keep|Head] ).

% timer: Repeatedly sends a message to the server to get a new board
% 	 from the backend, then sends that board to all the players. 
%
% Input: {ServerName, ServerNode}
%        ServerName: Name of the server running the game.
%        ServerNode: Name of the node the server is running on.
% Output: None. Loops repeatedly.
timer({ServerName, ServerNode}) ->
    gen_server:cast({ServerName, ServerNode}, {update_board}),
    timer:sleep(250),
    timer({ServerName, ServerNode}).

% sendboard: Send the given board to all users in the list of users.
%
% Input: List of {UserName, UserNode} tupples.
%        Board: Board state to send to users.
% Output: {ok}
send_board([], _Board) ->
    {ok};
send_board([{UserName, UserNode}], Board) ->
    {UserName, UserNode} ! {board, Board};
send_board([{UserName, UserNode} | Usernames], Board) ->
    {UserName, UserNode} ! {board, Board},
    send_board(Usernames, Board).
