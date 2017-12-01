import random
import Queue
import threading

BOARDWIDTH = 160
BOARDHEIGHT = 40

# MySnek class to instantiate a game of Snek and provide a series of function calls
#	to interact with it 
	
	# Init takes in the Node and PID of the calling server
def make_fields(Node, PID) :
	
	global board
	global players
	global snakes
	global server	
		# Server information stored in a tuple
	server = (Node, PID)
		
		# Board constructed as list with predefined parameters
	board = [[' ' for x in range(BOARDWIDTH)] \
		for y in range(BOARDHEIGHT)]
		
	seed = find_empty_spot()
	board[seed[0]][seed[1]] = '*'

	seed = find_empty_spot()
	board[seed[0]][seed[1]] = '@'


		# Current players and locations stored in Dict for fast access
		#   keyed on player (Node,PID), stores list:
		#   	[current_location, Char_Head_Token, Score, last_location]
	players = {} 

	snakes = []
		
		# ---We'll see if this is necessary - Queue and Threading might be heavier weight
		# ----than just handling async one step upstream with erl server
	
			# Queue for move requests 
		#  self.queue = Queue.Queue()
		# if need to implement threading this must be a cast or threaded return: 

		#return ('started', (Node,PID))

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

	# Function to add player to game. Takes in player Node, PID and generates a 
	#	random starting location
def add_player(Node, PID):

	global board
	global players
	global snakes
	global server

		# Check to see if player is already playing from a given PID
	if (Node,PID) not in players:

			# player seed loccation
		seed = find_empty_spot()

		snake_head = chr(ord('A'))
		while snake_head in snakes:
			snake_head = chr(ord(snake_head) + 1)
			players[(Node,PID)] = [seed, \
			snake_head, 0, seed, 50, []]
			snakes.append(snake_head)

			board[seed[0]][seed[1]] = snake_head

		return ('added', (Node,PID))


	# Basic Bookkeeping deletion from players Dict.
	#	Callable on player quit or death. 
def remove_player(Node, PID, head):

	global board
	global players
	global snakes
	global server

	if (Node,PID) in players:
		if len(players) == 1:
			return ('quit', server)
			
		else:
			del players[(Node,PID)]
			snakes.remove(head)
			return ('removed', (Node,PID))

	else:
		return ('removed', (Node,PID))

def _move_check(Node, PID, newP, oldP):
		
	global board
	global players
	global snakes
	global server

	pTup = (Node,PID)
		# Do nothing if trying to backtrack
	if newP[0] == oldP[0] and newP[1] == oldP[1]:
		return

	collis = board[newP[0]][newP[1]]
		
	if collis == '*':
		collis = ' '
		players[pTup][4] += 25
		seed = find_empty_spot()
		board[seed[0]][seed[1]] = '*'
			
	if collis == '@':
		collis = ' '
		players[pTup][4] += 20
		seed = find_empty_spot()
		board[seed[0]][seed[1]] = '*'	

	if collis != ' ':
			# crash - this kills the snek
		board[oldP[0]][oldP[1]] = ' '
		return remove_player(Node, PID)

	else:
			# make old head space lowercase
		board[oldP[0]][oldP[1]] = players[pTup][1].lower()
			# make new head space non-space
		board[newP[0]][newP[1]] = players[pTup][1]
			# remember last space
		players[pTup][3] = oldP
			#remember new space
		players[pTup][0] = newP

			# every move costs a point
		players[pTup][4] -= 1
		if players[pTup][4] == 0:
			return remove_player(Node, PID)

			# track the snake tail
		players[pTup][5].append(oldP)
		if len(players[pTup][5]) > 20:
			tail_rem = players[pTup][5].pop(0)
			board[tail_rem[0]][tail_rem[1]] = ' '

		return ('moved', (Node,PID))

def move(Node, PID, direc):

	global board
	global players
	global snakes
	global server

		# construction of player tuple for dict key access
	pTup = (Node,PID)
		# player dict access for current player location
	oldP = players[pTup][0]

		# Attempted to move up
	if (direc == 'w' or direc == 'W'):
				
	newP = ((oldP[0]-1)%BOARDHEIGHT,oldP[1])

		return _move_check(Node,PID,newP,oldP)

		# Attempted to move left
	elif (direc == 'a' or direc == 'A'):

		newP = (oldP[0],(oldP[1]-1)%BOARDWIDTH)			

		return _move_check(Node,PID,newP,oldP)

		# Attempted to move right
	elif (direc == 'd' or direc == 'D'):
			
		newP = (oldP[0],(oldP[1]+1)%BOARDWIDTH)

		return _move_check(Node,PID,newP,oldP)

	else:
			
			# Attempted to move down
		newP = ((oldP[0]+1)%BOARDHEIGHT,oldP[1])

		return _move_check(Node,PID,newP,oldP)

def get_board():

	global board
	global players
	global snakes
	global server

		return tuple(board)
		# TODO: Add powerup logic
		#  might include adding tail field to player data for death
		#def add_power_up(self):

# def makesname(Node, PID):
# 	global gamesnake = mysnek(Node,PID)
# 	return
