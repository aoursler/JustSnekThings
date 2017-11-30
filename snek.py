import random

BOARDWIDTH = 160
BOARDHEIGHT = 40

# instantiate a game of Snek and provide a series of function calls
#	to interact with it 
	

# make_fields(GameName): takes in a GameName associated with an erlang
#   gameserver and creates the fields necessary to play a game of snek
def make_fields(GameName) :
	
	global board
	global players
	global snakes
	global server	

	# Server information stored in a tuple
	server = (GameName)
		
	# Board constructed as list with predefined parameters
	board = [[' ' for x in range(BOARDWIDTH)] \
		for y in range(BOARDHEIGHT)]
		
	# finds an empty spot for a powerup
	seed = find_empty_spot()
	board[seed[0]][seed[1]] = '*'

	# finds an empty spot for another powerup
	seed = find_empty_spot()
	board[seed[0]][seed[1]] = '@'


	# Current players and locations stored in players array for fast access
	#   stores list:
	#   	[UserName, current_location, Char_Head_Token, Score, 
	#			last_locations, Power, Tail Locations]
	players = []

	# List  of currently used snake tokens
	snakes = []
		
	return ('started', (GameName))


# find_empty_spot(): Internal function to find empty spot on board for seeding powerups and 
#   new players
def find_empty_spot():
	
	global board
	global players
	global snakes
	global server

	seed = (random.randint(0, BOARDHEIGHT-1), \
				random.randint(0, BOARDWIDTH-1))
	# Reroll on seed conflict
	while board[seed[0]][seed[1]] != ' ':
		seed = (random.randint(0, BOARDHEIGHT-1), \
			random.randint(0, BOARDWIDTH-1))

	return seed


# add_player(UserName): Function to add player to game. Takes in UserName and 
#   generates a random starting location
def add_player(UserName):

	global board
	global players
	global snakes
	global server

	# Check to see if player is already playing from a UserName
	for i in players:
		if (UserName) == i[0]:
			return ('player already in game', (UserName))
	else:

		# player seed location
		seed = find_empty_spot()

		# create a new token for the snake
		snake_head = chr(ord('A'))
		pTup = (UserName)
		# reroll snake token if necessary
		while snake_head in snakes:
			snake_head = chr(ord(snake_head) + 1)
		players.append([pTup,seed, \
		snake_head, 0, seed, 50, []])
		snakes.append(snake_head)

		board[seed[0]][seed[1]] = snake_head
		
		return ('added', (UserName))


# remove_player(UserName): Basic Bookkeeping deletion from players list
#	Callable on player quit or death. 
def remove_player(UserName):

	global board
	global players
	global snakes
	global server

	# finds the players element to remove
	for i in players:
		if i[0] == (UserName):
			head = i[2]
			players.remove(i)
			snakes.remove(head)

	if len(players) == 0:
		return ('quit', server)

	else:
		return ('removed', (UserName))
	

def get_players():
	global players
	x = []
	for i in players:
		x.append(i)
	return players


def get_snakes():
	global snakes
	return snakes

# _move_check(UserName, newP, oldP): internal function that takes the UserName, 
#   new location and old location to check if the move is valid
def _move_check(UserName, newP, oldP):
		
	global board
	global players
	global snakes
	global server

	pTup = (UserName)
	# Do nothing if trying to backtrack
	if newP[0] == oldP[0] and newP[1] == oldP[1]:
		return

	collis = board[newP[0]][newP[1]]
	
	# collision with powerup	
	if collis == '*':
		collis = ' '
		for i in players:
			if i[0] == (UserName):
				i[5] += 25
		seed = find_empty_spot()
		board[seed[0]][seed[1]] = '*'
			
	# collision with powerup		
	if collis == '@':
		collis = ' '
		for i in players:
			if i[0] == (UserName):
				i[5] += 20
		seed = find_empty_spot()
		board[seed[0]][seed[1]] = '*'	

	# collision with other filled space
	if collis != ' ':
		# crash - this kills the snek
		board[oldP[0]][oldP[1]] = ' '
		return remove_player(UserName)

	else:
		temparray = None
		for i in players:
			if i[0] == (UserName):
				temparray = i
		# make old head space lowercase
		board[oldP[0]][oldP[1]] = temparray[2].lower()
		# make new head space non-space
		board[newP[0]][newP[1]] = temparray[2]
			# remember last space
		temparray[4] = oldP
			#remember new space
		temparray[1] = newP

		# every move costs a point
		temparray[5] -= 1
		if temparray[5] == 0:
			return remove_player(UserName)

		# track the snake tail
		temparray[6].append(oldP)
		if len(temparray[6]) > 20:
			tail_rem = temparray[6].pop(0)
			board[tail_rem[0]][tail_rem[1]] = ' '

		# replace the players element with the updated values
		for i in players:
			if i[0] == (UserName):
				x = players.index(i)
				players[x] = temparray

		return ('moved', (UserName))

# move(UserName, direc): A function to take in a player and a move and digest it in
#   the game of snek
def move(UserName, direc):

	global board
	global players
	global snakes
	global server

	# construction of player tuple for players array access
	pTup = (UserName)
	# players array access for current player location
	oldP = None
	for i in players:
		if i[0] == (UserName):
			oldP = i[1]

	# Attempted to move up
	if (direc == 'w' or direc == 'W'):
				
		newP = ((oldP[0]-1)%BOARDHEIGHT,oldP[1])

		return _move_check(UserName,newP,oldP)

	# Attempted to move left
	elif (direc == 'a' or direc == 'A'):

		newP = (oldP[0],(oldP[1]-1)%BOARDWIDTH)			

		return _move_check(UserName,newP,oldP)

	# Attempted to move right
	elif (direc == 'd' or direc == 'D'):
			
		newP = (oldP[0],(oldP[1]+1)%BOARDWIDTH)

		return _move_check(UserName,newP,oldP)

	else:
			
		# Attempted to move down
		newP = ((oldP[0]+1)%BOARDHEIGHT,oldP[1])

		return _move_check(UserName,newP,oldP)

def get_board():

	global board
	global players
	global snakes
	global server

	return {'board', tuple(board)}
		# TODO: Add powerup logic
		#  might include adding tail field to player data for death
		#def add_power_up(self):
