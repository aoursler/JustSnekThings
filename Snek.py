import random
import Queue
import threading

BOARDWIDTH = 160
BOARDHEIGHT = 40

# MySnek class to instantiate a game of Snek and provide a series of function calls
#	to interact with it 
class mysnek:
	
	# Init takes in the Node and PID of the calling server
	def __init__(self, Node, PID) :
		
		# Server information stored in a tuple
		self.server = (Node, PID)
		
		# Board constructed as list with predefined parameters
		self.board = [[' ' for x in range(BOARDWIDTH)] \
			for y in range(BOARDHEIGHT)]
		
		seed = self.find_empty_spot()
		self.board[seed[0]][seed[1]] = '*'

		seed = self.find_empty_spot()
		self.board[seed[0]][seed[1]] = '@'


		# Current players and locations stored in Dict for fast access
		#   keyed on player (Node,PID), stores list:
		#   	[current_location, Char_Head_Token, Score, last_location]
		self.players = {} 

		self.snakes = []
		
		# ---We'll see if this is necessary - Queue and Threading might be heavier weight
		# ----than just handling async one step upstream with erl server
	
			# Queue for move requests 
		#  self.queue = Queue.Queue()
		# if need to implement threading this must be a cast or threaded return: 

		#return ('started', (Node,PID))

	def find_empty_spot(self):
		seed = (random.randint(0, BOARDHEIGHT-1), \
				random.randint(0, BOARDWIDTH-1))
			# Reroll on seed conflict
		while self.board[seed[0]][seed[1]] != ' ':
			seed = (random.randint(0, BOARDHEIGHT-1), \
				random.randint(0, BOARDWIDTH-1))

		return seed

	# Function to add player to game. Takes in player Node, PID and generates a 
	#	random starting location
	def add_player(self, Node, PID):
		
		# Check to see if player is already playing from a given PID
		if (Node,PID) not in self.players:

			# player seed loccation
			seed = self.find_empty_spot()

			snake_head = chr(ord('A'))
			while snake_head in self.snakes:
				snake_head = chr(ord(snake_head) + 1)
			self.players[(Node,PID)] = [seed, \
			snake_head, 0, seed, 50, []]
			self.snakes.append(snake_head)

			self.board[seed[0]][seed[1]] = snake_head

		return ('added', (Node,PID))


	# Basic Bookkeeping deletion from players Dict.
	#	Callable on player quit or death. 
	def remove_player(self, Node, PID, head):

		if (Node,PID) in self.players:
			if len(self.players) == 1:
				return ('quit', self.server)
			
			else:
				del self.players[(Node,PID)]
				self.snakes.remove(head)
				return ('removed', (Node,PID))

		else:
			return ('removed', (Node,PID))

	def _move_check(self, Node, PID, newP, oldP):
		
		pTup = (Node,PID)
		# Do nothing if trying to backtrack
		if newP[0] == oldP[0] and newP[1] == oldP[1]:
			return

		collis = self.board[newP[0]][newP[1]]
		
		if collis == '*':
			collis = ' '
			self.players[pTup][4] += 25
			seed = self.find_empty_spot()
			self.board[seed[0]][seed[1]] = '*'
			
		if collis == '@':
			collis = ' '
			self.players[pTup][4] += 20
			seed = self.find_empty_spot()
			self.board[seed[0]][seed[1]] = '*'	

		if collis != ' ':
			# crash - this kills the snek
			self.board[oldP[0]][oldP[1]] = ' '
			return self.remove_player(Node, PID)

		else:
			# make old head space lowercase
			self.board[oldP[0]][oldP[1]] = self.players[pTup][1].lower()
			# make new head space non-space
			self.board[newP[0]][newP[1]] = self.players[pTup][1]
			# remember last space
			self.players[pTup][3] = oldP
			#remember new space
			self.players[pTup][0] = newP

			# every move costs a point
			self.players[pTup][4] -= 1
			if self.players[pTup][4] == 0:
				return self.remove_player(Node, PID)

			# track the snake tail
			self.players[pTup][5].append(oldP)
			if len(self.players[pTup][5]) > 20:
				tail_rem = self.players[pTup][5].pop(0)
				self.board[tail_rem[0]][tail_rem[1]] = ' '

			return ('moved', (Node,PID))

	def move(self, Node, PID, direc):

		# construction of player tuple for dict key access
		pTup = (Node,PID)
		# player dict access for current player location
		oldP = self.players[pTup][0]

		# Attempted to move up
		if (direc == 'w' or direc == 'W'):
				
			newP = ((oldP[0]-1)%BOARDHEIGHT,oldP[1])

			return self._move_check(Node,PID,newP,oldP)

		# Attempted to move left
		elif (direc == 'a' or direc == 'A'):

			newP = (oldP[0],(oldP[1]-1)%BOARDWIDTH)			

			return self._move_check(Node,PID,newP,oldP)

		# Attempted to move right
		elif (direc == 'd' or direc == 'D'):
			
			newP = (oldP[0],(oldP[1]+1)%BOARDWIDTH)

			return self._move_check(Node,PID,newP,oldP)

		else:
			
			# Attempted to move down
			newP = ((oldP[0]+1)%BOARDHEIGHT,oldP[1])

			return self._move_check(Node,PID,newP,oldP)

		def get_board(self):

			return tuple(self.board)
		# TODO: Add powerup logic
		#  might include adding tail field to player data for death
		#def add_power_up(self):

def makesname(Node, PID):
	global gamesnake = mysnek(Node,PID)
	return
