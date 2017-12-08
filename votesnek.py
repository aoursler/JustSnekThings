"""

votesnek.py

Tufts Comp 50CP - Group Project

Team: JustSnekThings
Members: Anne Oursler, Lexi Galantino, Matt Turner

"""

import random
from erlport.erlterms import Atom

BOARDWIDTH = 50
BOARDHEIGHT = 50

# instantiate a game of Snek and provide a series of function calls
#	to interact with it 
global board
global player
global snakes
global server
global users
global moves	

def make_fields( GameName, GameNode ):
	"""
	make_fields( GameName, GameNode ): 

		Interface function which takes in a GameName associated
		with an erlang gameserver and creates the fields 
		necessary to play a game of snek.

		Inputs:		GameName: An input gameserver name
				GameNode: The gameserver's erlang node
		Outputs:	A callback tuple to the calling erlang
				process of successful game instantiation
	"""	

	global board
	global player
	global users
	global snakes
	global server	
	global moves

	# Server information stored in a tuple
	server = ( GameName, GameNode )
		
	# Board constructed as list with predefined parameters
	board = [ [' ' for x in range( BOARDWIDTH ) ] \
		for y in range( BOARDHEIGHT ) ]
		
	# finds an empty spot for a powerup
	for i in range( 5 ):
		seed = find_empty_spot()
		board[ seed[ 0 ] ][ seed[ 1 ] ] = '*'

	# finds an empty spot for another powerup
	for i in range( 5 ):
		seed = find_empty_spot()
		board[ seed[ 0 ] ][ seed[ 1 ] ] = '@'

	users = []

	# player seed location
	seed = find_empty_spot()

	# create a new token for the snake
	snake_head = chr( ord( 'A' ) )

	
	# adds snake data to the global state variable
	# Current player and locations stored in player array for fast 
	# access player list is of the form:
	#   	[ Current_Location, Char_Head_Token, Score,
	#			Last_Location, Power, Tail_Locations ]
	player = [ seed, snake_head, 0, seed, 50, [] ] 
	
	snakes = [ snake_head ]
	
	board[ seed[ 0 ] ][ seed[ 1 ] ] = snake_head
	
	# List  of currently used snake tokens
	moves = []
		
	return ( Atom( 'started' ), \
		( Atom( GameName ), Atom( GameNode ) ) )


def find_empty_spot():
	"""
	find_empty_spot(): 

		Internal function which finds and returns an empty spot 
		on the global board to be used for seeding of players and
		powerups

		Outputs:	A tuple representing the 2d array index 
				of the free space
	"""	

	global board
	global player
	global users
	global snakes
	global server

	seed = ( random.randint( 0, BOARDHEIGHT - 1 ), \
				random.randint( 0, BOARDWIDTH - 1 ) )
	# Reroll on seed conflict
	while board[ seed[ 0 ] ][ seed[ 1 ] ] != ' ':
		seed = ( random.randint( 0, BOARDHEIGHT - 1 ), \
			random.randint( 0, BOARDWIDTH - 1 ) )

	return seed


def add_player( UserName, UserNode ):
	"""
	add_player( UserName, UserNode ):

		Interface function which generates all bookkeeping and
		updates all global variables necessary to add a new user
		to the game.

		Inputs:		UserName: An input user name
				UserNode: The user's erlang node
		Outputs:	A callback tuple to the calling erlang
				process of successful user addition
	"""	

	global board
	global player
	global users
	global snakes
	global server
	global moves

	# Check to see if player is already playing from a UserName
	for i in users:
		if ( UserName, UserNode ) == i[ 0 ]:
			return ( Atom( 'duplicate' ), \
				( Atom( UserName ), Atom( UserNode ) ) )

	users.append( ( UserName, UserNode ) )
	
	return ( Atom( 'added' ), ( Atom( UserName ), Atom( UserNode ) ) )


def remove_player( UserName, UserNode ):
	"""
	remove_player( UserName, UserNode ): 

		Internally and externally accessed functionn which 
		handles bookkeeping deletion from users list. Callable 
		on player quit or death. 

		Inputs:		UserName: An input user name
				UserNode: The user's erlang node
		Outputs:	A callback tuple to the calling erlang
				process of successful player removal
	"""

	global board
	global player
	global snakes
	global server
	global users
	global moves

	# finds the user element to remove
	for i in users:
		if i == ( UserName, UserNode ):
			users.remove( i )

	# no more players left: game exit
	if len( users ) == 0:
		return ( Atom( 'serverQuit' ), \
			( Atom( UserName ), Atom( UserNode ) ) )

	else:
		return ( Atom( 'removed' ), \
			( Atom( UserName ), Atom( UserNode ) ) )


def get_players():
	"""
	get_players():

		Debugging function to check proper gamestate maintenance 
		of player table

		Outputs: 	The full player information list
	"""

	global player
	return player


def get_snakes():
	"""
	get_snakes():

		Debugging function to check proper gamestate maintenance 
		on snake data

		Outputs:	A list of snake data
	"""

	global snakes
	return snakes


def _move_check( UserName, UserNode, newP, oldP ):
	"""
	_move_check( UserName, UserNode, newP, oldP ):

		An internal function to check whether a move from input 
		oldP location to input newP location is valid for the
		given player.

		Inputs:	UserName:	The name of the last moving user
			UserNode:	The node of the last moving user
			newP: 		The calculated move destination
			oldP:		The calculated previous move 
					location
		Outputs:	On success, an erlang callback indicating
				that the player moved. On failure, an
				internal call to the remove_player 
				function, which then handles bookkeeping
				cleanup.
	""" 
		
	global board
	global player
	global snakes
	global server
	global users
	global moves

	temparray = player

	# Do nothing if trying to backtrack
	if newP[ 0 ] == temparray[ 3 ][ 0 ] and \
		newP[ 1 ] == temparray[ 3 ][ 1 ]:
		return ( Atom( 'moved' ), \
			( Atom( UserName ), Atom( UserNode ) ) )

	collis = board[ newP[ 0 ] ][ newP[ 1 ] ]
	
	# collision with powerup	
	if collis == '*':
		collis = ' '
		temparray[ 4 ] += 25
		seed = find_empty_spot()
		board[ seed[ 0 ] ][ seed[ 1 ] ] = '*'
			
	# collision with powerup		
	elif collis == '@':
		collis = ' '		
		temparray[ 4 ] += 20
		seed = find_empty_spot()
		board[ seed[ 0 ] ][ seed[ 1 ] ] = '*'	

	# collision with other filled space
	if collis != ' ':
		# crash - this kills the snek
		board[ oldP[ 0 ] ][ oldP[ 1 ] ] = ' '
		return ( Atom( 'serverQuit' ), ( Atom( UserName ), \
				Atom( UserNode ) ) )

	else:
		# make old head space lowercase
		board[ oldP[ 0 ] ][ oldP[ 1 ] ] = temparray[ 1 ].lower()
		# make new head space non-space
		board[ newP[ 0 ] ][ newP[ 1 ] ] = temparray[ 1 ]
		# remember last space
		temparray[ 3 ] = oldP
		#remember new space
		temparray[ 0 ] = newP

		# every move costs a point
		temparray[ 4 ] -= 1
		if temparray[ 4 ] == 0:
			return ( Atom( 'serverQuit' ), \
				( Atom( UserName ), Atom( UserNode ) ) )

		# track the snake tail
		temparray[ 5 ].append( oldP )
		if len( temparray[ 5 ] ) > 20:
			tail_rem = temparray[ 5 ].pop( 0 )
			board[ tail_rem[ 0 ] ][ tail_rem[ 1 ] ] = ' '

		user = temparray

		return ( Atom( 'moved' ), \
			( Atom( UserName ), Atom( UserNode ) ) )


def move( UserName, UserNode, direc ):
	"""
	move( UserName, UserNode, direc ):

		An interface function called by an external erlang 
		process to feed player moves to the game for digestion.
		Move returns a call to an internal function _move_check,
		which handles the logic and validity of the move

		Inputs:	UserName: 	The name of the user, for key 
					access to player data
			UserNode: 	The name of the user's node, for 
					key access to player data
			direc:		The direction in which to move
		Outputs: 	A function call to check the move validity
	"""

	global board
	global player
	global snakes
	global server
	global users
	global moves

	if ( direc == 'quit' ):

		return remove_player( UserName, UserNode )

	else:
		
		moves.append( direc )

	if len( moves ) < 20:
		
		return ( Atom( 'ok' ), \
			( Atom( UserName ), Atom( UserNode ) ) )

	else:

		# find the most voted for move
		direc = max( moves, key = moves.count )
		# reset the moves buffer
		moves = []

	oldP = player[ 0 ]

	# Attempted to move up
	if ( direc == 'w' or direc == 'W' ):
				
		newP = ( ( oldP[ 0 ] - 1 )%BOARDHEIGHT, oldP[ 1 ] )

		return _move_check( UserName, UserNode,newP, oldP )

	# Attempted to move left
	elif ( direc == 'a' or direc == 'A' ):

		newP = ( oldP[ 0 ], ( oldP[ 1 ] - 1 )%BOARDWIDTH )	

		return _move_check( UserName, UserNode,newP, oldP )

	# Attempted to move right
	elif ( direc == 'd' or direc == 'D' ):
			
		newP = ( oldP[ 0 ],( oldP[ 1 ] + 1 )%BOARDWIDTH )

		return _move_check( UserName, UserNode, newP, oldP )

	else:
			
		# Attempted to move down
		newP = ( ( oldP[ 0 ] + 1 )%BOARDHEIGHT, oldP[ 1 ] )

		return _move_check( UserName, UserNode, newP, oldP )

def get_board():
	"""
	get_board():

		An interface function called by an external erlang process
		to feed the current board state to the erlang network, 
		which then distributes that information to the clients.

		Outputs:	A tuple of a 2d list of the board state
	"""

	global board

	return tuple( board )
