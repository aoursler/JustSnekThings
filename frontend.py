# 
# frontend.py
#
# Tufts Comp 50CP - Group Project
#
# Team: JustSnekThings
# Members: Anne Oursler, Lexi Galantino, Matt Turner
#

from Tkinter import *
from erlport.erlterms import Atom
from erlport.erlang import cast
from erlport.erlang import call
from sys import *
import time

# Declared global variables for GUI
global tokenlist 
global colors 
global boardWidth 
global boardHeight

# Instantiated values
tokenlist = [' ','*','@','A','a','B','b','C','c'] 
colors = ['white','spring green','lawn green','maroon2','maroon3','OliveDrab4','OliveDrab1','MediumPurple4','MediumPurple1']
boardWidth = 50
boardHeight = 50

# GUI class which instantiates a tkinter instance and loops it
class snekGUI:
    def __init__(self, width, height, server):
        global boardWidth
        global boardHeight
        self.server = server
        self.root = Tk()
        self.root.geometry('751x751')
        self.root.title("JustSnekThings")
        self.canvas = Canvas(self.root, width=boardWidth*width, \
            height=boardHeight*height)
        self.root.bind('<Left>', self.left)
        self.root.bind('<Right>', self.right)
        self.root.bind('<Up>', self.up)
        self.root.bind('<Down>', self.down)
        self.root.bind('q', self.quit)
        self.canvas.pack()
	
        self.root.after(100, self.get_board)
        cast(self.server, Atom("link"))
        self.root.mainloop()

    # Internal key mapping to left arrow key
    def left(self, _):
        cast(self.server, Atom("left"))
    # Internal key mapping to right arrow key
    def right(self, _):
        cast(self.server, Atom("right"))
    # Internal key mapping to up arrow key
    def up(self, _):
        cast(self.server, Atom("up"))
    # Internal key mapping to down arrow key
    def down(self, _):
        cast(self.server, Atom("down"))
    # Internal key mapping for GUI exit
    def quit(self, _):
        cast(self.server, Atom("quit"))
        time.sleep(1)
        self.root.destroy()
        sys.exit(0)

    # Board update function to call client erlang process
    def get_board(self):
        board = call(Atom("just_snek_things"), Atom("get_board"), \
            [self.server])
        #print board
        if board == Atom("die"):
            time.sleep(1)
            self.root.destroy()
            sys.exit(0)            
        
        self.print_board(board)
        self.root.after(200, self.get_board)

    # Internal function to print latest board
    def print_board(self, board):
        global tokenlist
        global colors

        for row in range(len(board)):
            for col in range(len(board[row])):
                pix = board[row][col]
                color = tokenlist.index(pix)
                self.canvas.create_rectangle(col*15, row*15, (col+1)*15, \
                 (row+1)*15, fill=colors[color])
