% Just Sneck Things
%
% Writen by: Anne Oursler, November 2017
% Code Reviewed by: Matt Turner

%TODO: Remove print statements from this code. They are for debugging purposes
%      before we have a frontend

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
    %io:fwrite("in join game~n"),
    %io:fwrite("~w~n", [ServerName]),
    %io:fwrite("~w~n", [ServerNode]),
    %io:fwrite("~w~n", [UserName]),
    %io:fwrite("~w~n", [UserNode]),

    {ok, Pfront} = python:start([{python_path, "/"}]),
    % subscribes to the given Game on the given Host with the given UserName
    subscribe( ServerName, ServerNode, UserName, UserNode ),
    %io:fwrite("got out of subscribe~n"),
    % resizes the window to BoardHeight x BoardWidth
    %io:fwrite("\e[8;~w;~wt", [boardHeight(), boardWidth()]),
    % registers UserName to the PID of the server->client receive loop
    ServerPid = spawn_link(node(), ?MODULE, move, [ServerName, ServerNode, UserName, UserNode, Pfront, {[]}] ),

    register( UserName, ServerPid),

    %io:fwrite("joined_game~n"),
    % starts a move loop which will persist until client exit

  % starts the frontend
    python:call(Pfront, frontend, snekGUI, [boardWidth(), boardHeight(), ServerPid]).

    % TODO: (lexi probs) figure out how to unsubscribe successfully
    % once move loop ends, player is done: unsubscribe
    % unsubscribe( ServerName, ServerNode, UserName, UserNode ).

% move( GameName, HostName, UserName, UserNode ): main loop to receive moves
%   from python client to pass through to server
move( ServerName, ServerNode, UserName, UserNode, Pfront, Board) ->
    %io:fwrite("got to move~n"),
    receive
        link -> link(Pfront),
                move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );    

        { board, NewBoard } ->
            %lists:map(fun(X) -> io:fwrite("~w~n", [X]) end, tuple_to_list(NewBoard)),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, NewBoard );

        { getBoard, Sender } ->
            Sender ! Board,
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board);
        up ->
            gen_server:cast( { ServerName, ServerNode },
                { move_up, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );

        left ->
            gen_server:cast( { ServerName, ServerNode },
                { move_left, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );

        down ->
            gen_server:cast( { ServerName, ServerNode },
                { move_down, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );

        right ->
            gen_server:cast( { ServerName, ServerNode },
                { move_right, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );

        quit ->
            gen_server:cast( { ServerName, ServerNode },
                { quit, { UserName, UserNode } } ),
            move( ServerName, ServerNode, UserName, UserNode, Pfront, Board );    
            %timer:sleep(1000),
            %exit(kill);A

        _Move ->
            io:fwrite("ERROR, BAD INPUT, NOTHING SENT TO SERVER~n")
    end.

% This function fetches the board for the python front end by making a call to
% the move function. It is the interface between the erlang server and the
% python frontend.
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
% TODO: figure out whether this is redundant
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
    %io:fwrite( "server_started~n" ),
    %io:fwrite( "~w~n", [Pname]),
    % GameName registered to gameserver PID
    %register( GameName, Pid ),
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
    %io:fwrite("subscribing~n"),
    %timer:sleep(5000),
    %io:fwrite("~w~n", [Pname]),
    %io:fwrite("~w~n", [LoopData]),
    %io:fwrite("~w~n", [UserName]),
    %io:fwrite("~w~n", [UserNode]),
    _Response = python:call( Pname, snek, add_player, [ UserName, UserNode ] ),
    %io:fwrite("~w~n", [Response]),
    %timer:sleep(5000),
    %io:fwrite( "getting out of python call~n"),
    { noreply, { [Pname], [ { UserName,UserNode } | LoopData ] } };

% handle_cast(unsubscribe): takes in UserName and UserNode for unsubscription,
% %   removes player from python game, updates state
handle_cast( { unsubscribe, {UserName, UserNode } }, { [Pname], LoopData } ) ->
    %io:fwrite("unsubscribing~n"),
    {Reply, {Uname,Unode}} = python:call( Pname, snek, remove_player, [ UserName, UserNode ] ),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], filterOut( { UserName, UserNode }, LoopData ) } };

% handle_cast(moves): sends UserName, UserNode and move to python game
%  TODO: from/for Matt: python call returns failure tuple on death. integrate.
handle_cast( { quit, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, quit] ),
    io:fwrite("~w~n", [Reply]),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    { noreply, { [Pname], LoopData } };

handle_cast( { move_left, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, a] ),
    io:fwrite("~w~n", [Reply]),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    %io:fwrite("moved left ~n"),
    { noreply, { [Pname], LoopData } };
handle_cast( { move_right, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    %io:fwrite("moving right ~n"),
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, d] ),
    io:fwrite("~w~n", [Reply]),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    %io:fwrite("moved right ~n"),
    { noreply, { [Pname], LoopData } };
handle_cast( { move_up, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    %io:fwrite("moving up ~n"),
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, w] ),
    io:fwrite("~w~n", [Reply]),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _Reply -> ok
    end,
    %io:fwrite("moved up ~n"),
    { noreply, { [Pname], LoopData } };
handle_cast( { move_down, { UserName, UserNode } }, { [Pname], LoopData } ) ->
    %io:fwrite("moving down ~n"),
    {Reply, {Uname,Unode}} = python:call( Pname, snek, move, [UserName, UserNode, s] ),
    io:fwrite("~w~n", [Reply]),
    USName = rpc:call(Unode,erlang,whereis,[Uname]),
    case Reply of 
        removed -> rpc:cast(Unode, erlang, exit, [USName, kill]);
        serverQuit -> rpc:cast(Unode, erlang, exit, [USName, kill]),
                        exit( self(), kill);
        _reply -> ok
    end,
    %io:fwrite("moved down ~n"),
    { noreply, { [Pname], LoopData } };

% handle_cast(update_board): takes in UserName and UserNode and triggers,
%    a call to snek for the board and send the board to all of the users
handle_cast({update_board }, { [Pname], LoopData } ) ->
    io:fwrite("requesting board update ~n"),
    Board = python:call(Pname, snek, get_board, []),
    %lists:map(fun(X) -> io:fwrite("~w~n",[X]) end, tuple_to_list(Board)),
    io:fwrite("sending_board ~n"),
    send_board(LoopData, Board),
    {noreply, { [Pname], LoopData}}.

% handle_call/3: stub for handle_call
handle_call( _Request, _From, State ) ->
    { reply, State }.

% terminate/2 is an internal function called by the gen_server's stop()
%   upon exiting to terminate all connected chat client processes.
terminate( kill, _LoopData ) ->
    %io:fwrite("server_stopping~n"),
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

%frontendDie( ServerName, )
timer({ServerName, ServerNode}) ->
    gen_server:cast({ServerName, ServerNode}, {update_board}),
    timer:sleep(250),
    timer({ServerName, ServerNode}).

send_board([], _Board) ->
    {ok};
send_board([{UserName, UserNode}], Board) ->
    {UserName, UserNode} ! {board, Board};
send_board([{UserName, UserNode} | Usernames], Board) ->
    {UserName, UserNode} ! {board, Board},
    send_board(Usernames, Board).
