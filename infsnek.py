"""

infsnek.py

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
global players
global snakes
global server
	

def make_fields( GameName, GameNode ):
	"""
	make_fields( GameName, GameNode ): 

		Interface function which takes in a GameName associated with an erlang
		gameserver and creates the fields necessary to play a game of snek.

		Inputs:		GameName: An input gameserver name
					GameNode: The gameserver's erlang node
		Outputs:	A callback tuple to the calling erlang process of successful 
 					game instantiation
	"""

	global board
	global players
	global snakes
	global server	

	# Server information stored in a tuple
	server = ( GameName, GameNode )
		
	# Board constructed as list with predefined parameters
	board = [ [' ' for x in range( BOARDWIDTH ) ] \
		for y in range( BOARDHEIGHT ) ]
		
	# finds an empty spot for a powerup
	for i in range( 3 ):
		seed = find_empty_spot()
		board[ seed[ 0 ] ][ seed[ 1 ] ] = '*'

	# finds an empty spot for another powerup
	for i in range( 3 ):
		seed = find_empty_spot()
		board[ seed[ 0 ] ][ seed[ 1 ] ] = '@'


	# Current players and locations stored in players array for fast access
	#   individual player sublists are of the form:
	#   	[ ( UserName, UserNode ), current_location, Char_Head_Token, Score,
	#			last_locations, Power, Tail Locations ]
	players = []

	# List  of currently used snake tokens
	snakes = []
		
	return ( Atom( 'started' ), ( Atom( GameName ), Atom( GameNode ) ) )


def find_empty_spot():
	"""
	find_empty_spot(): 

		Internal function which finds and returns an empty spot on the global
		board to be used for seeding of players and powerups

		Outputs:	A tuple representing the 2d array index of the free space
	"""	

	global board
	global players
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

		Interface function which generates all bookkeeping and updates all global
		variables necessary to add a new player to the game.

		Inputs:		UserName: An input user name
					UserNode: The user's erlang node
		Outputs:	A callback tuple to the calling erlang process of successful 
	 				player addition
	"""	

	global board
	global players
	global snakes
	global server

	# Check to see if player is already playing from a UserName
	for i in players:
		if ( UserName, UserNode ) == i[ 0 ]:
			return ( Atom( 'duplicate' ), ( Atom( UserName ), \
				Atom( UserNode ) ) )
	

	# player seed location
	seed = find_empty_spot()
	# create a new token for the snake
	snake_head = chr( ord( 'A' ) )
	pTup = ( UserName, UserNode )
	# reroll snake token if necessary
	while snake_head in snakes:
		snake_head = chr( ord( snake_head ) + 1 )
	# adds player data to the global state variable
	players.append( [ pTup,seed, \
	snake_head, 0, seed, 50, [] ] )
	
	snakes.append( snake_head )
	board[ seed[ 0 ] ][ seed[ 1 ] ] = snake_head
	
	# add extra powerups whenever a new player joins	
	seed = find_empty_spot()
	board[ seed[ 0 ] ][ seed[ 1 ] ] = '*'

	seed = find_empty_spot()
	board[ seed[ 0 ] ][ seed[ 1 ] ] = '@'

		
	return ( Atom( 'added' ), ( Atom( UserName ),Atom( UserNode ) ) )


def remove_player( UserName, UserNode ):
	"""
	remove_player( UserName, UserNode ): 

		Internally and externally accessed functionn which handles bookkeeping 
		deletion from players list. Callable on player quit or death. 

		Inputs:		UserName: An input user name
					UserNode: The user's erlang node
		Outputs:	A callback tuple to the calling erlang process of successful 
	  				player removal
	"""

	global board
	global players
	global snakes
	global server

	# finds the players element to remove
	for i in players:
		if i[ 0 ] == ( UserName, UserNode ):
			head = i[ 2 ]

			# removing the tail
			for j in i[ 6 ]:
				board[ j[ 0 ] ][ j[ 1 ] ] = ' '
			# removing the head
			board[ i[ 1 ][ 0 ] ][ i[ 1 ][ 1 ] ] = ' '
			
			players.remove( i )
			snakes.remove( head )

	# no more players left: game exit
	if len( players ) == 0:
		return ( Atom( 'serverQuit' ), ( Atom( UserName ), Atom( UserNode ) ) )

	else:
		return ( Atom( 'removed' ), ( Atom( UserName ), Atom( UserNode ) ) )


def get_players():
	"""
	get_players():

		Debugging function to check proper gamestate maintenance of player table

		Outputs: 	The full player information list
	"""

	global players
	return players


def get_snakes():
	"""
	get_snakes():

		Debugging function to check proper gamestate maintenance on snake data

		Outputs:	A list of snake data
	"""

	global snakes
	return snakes


def _move_check( UserName, UserNode, newP, oldP ):
	"""
	_move_check( UserName, UserNode, newP, oldP ):

		An internal function to check whether a move from input oldP location
		to input newP location is valid for the given player.

		Inputs:		UserName:	The name of the moving user
					UserNode:	The node of the moving user
					newP: 		The calculated move destination
					oldP:		The calculated previous move location
		Outputs:	On success, an erlang callback indicating that the player
					moved. On failure, an internal call to the remove_player
					function, which then handles bookkeeping cleanup.
	""" 
		
	global board
	global players
	global snakes
	global server

	pTup = ( UserName, UserNode )
	temparray = None
	for i in players:
		if i[ 0 ] == pTup:
			temparray = i

	# Do nothing if trying to backtrack
	if newP[ 0 ] == temparray[ 4 ][ 0 ] and newP[ 1 ] == temparray[ 4 ][ 1 ]:
		return ( Atom( 'moved' ), pTup )

	collis = board[ newP[ 0 ] ][ newP[ 1 ] ]
	
	# collision with powerup	
	if collis == '*':
		collis = ' '
		temparray[ 5 ] += 25
		seed = find_empty_spot()
		board[ seed[ 0 ] ][ seed[ 1 ] ] = '*'
			
	# collision with powerup		
	elif collis == '@':
		collis = ' '		
		temparray[ 5 ] += 20
		seed = find_empty_spot()
		board[ seed[ 0 ] ][ seed[ 1 ] ] = '*'	

	# collision with other filled space
	if collis != ' ':
		# crash - this kills the snek
		board[ oldP[ 0 ] ][ oldP[ 1 ] ] = ' '

		return remove_player( UserName, UserNode )

	else:
		# make old head space lowercase
		board[ oldP[ 0 ] ][ oldP[ 1 ] ] = temparray[ 2 ].lower()
		# make new head space non-space
		board[ newP[ 0 ] ][ newP[ 1 ] ] = temparray[ 2 ]
			# remember last space
		temparray[ 4 ] = oldP
			#remember new space
		temparray[ 1 ] = newP

		# every move costs a point
		temparray[ 5 ] -= 1
		if temparray[ 5 ] == 0:
			return remove_player( UserName, UserNode )

		# track the snake tail
		temparray[ 6 ].append( oldP )
		
		# replace the players element with the updated values
		for i in players:
			if i[ 0 ] == ( UserName, UserNode ):
				x = players.index( i )
				players[ x ] = temparray

		return ( Atom( 'moved' ), ( Atom( UserName ), Atom( UserNode ) ) )


def move( UserName, UserNode, direc ):
	"""
	move( UserName, UserNode, direc ):

		An interface function called by an external erlang process to feed player
		moves to the game for digestion. Move returns a call to an internal
		function _move_check, which handles the logic and validity of the move

		Inputs:		UserName: 	The name of the user, for key access to player data
					UserNode: 	The name of the user's node, for key access to
								player data
					direc:		The direction in which to move
		Outputs: 	A function call to check the move validity
	"""

	global board
	global players
	global snakes
	global server

	# construction of player tuple for players array access
	pTup = ( UserName, UserNode )

	# players array access for current player location
	oldP = None
	for i in players:
		if i[ 0 ] == pTup:
			oldP = i[ 1 ]

	if oldP == None:
		return ( remove_player( UserName, UserNode ) )

	if ( direc == 'quit' ):

		return remove_player( UserName, UserNode )

	# Attempted to move up
	if ( direc == 'w' or direc == 'W' ):
				
		newP = ( ( oldP[ 0 ] - 1 )%BOARDHEIGHT, oldP[ 1 ] )

		return _move_check( UserName, UserNode, newP, oldP )

	# Attempted to move left
	elif ( direc == 'a' or direc == 'A' ):

		newP = ( oldP[ 0 ], ( oldP[ 1 ] - 1 )%BOARDWIDTH )			

		return _move_check( UserName, UserNode, newP, oldP )

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

		An interface function called by an external erlang process to feed
		the current board state to the erlang network, which then distributes
		that information to the clients.

		Outputs:	A tuple of a 2d list of the board state
	"""

	global board
	global players
	global snakes
	global server

	return tuple( board )
