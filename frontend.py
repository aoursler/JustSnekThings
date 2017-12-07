from Tkinter import *
from erlport.erlterms import Atom
from erlport.erlang import cast
from erlport.erlang import call
import threading

global tokenlist 
tokenlist = [' ','*','@',\
	     'A','a','B','b','C','c',\
	     'D','d','E','e','F','f',\
             'G','g','H','h','I','i',\
             'J','j','K','k','L','l',\
             'M','m','N','n','O','o',\
             'P','p','Q','q','R','r',\
             'S','s','T','t','U','u',\
             'V','v','W','w','X','x',\
	     'Y','y','Z','z']
global colors
colors = ['white','spring green','dark green',\
	  'maroon2','maroon3','OliveDrab4','OliveDrab1','MediumPurple4','MediumPurple1'\
          'deep sky blue','sky blue','dark orchid','medium orchid','gold4','gold3'\
          'PaleTurquoise4','PaleTurquoise3','blue4','blue2','maroon','pale violet red'\
          'yellow','light yellow','sienna4','sienna3','cyan','turquoise'\
          'dark orange', 'orange','seashell4','seashell3','LightSteelBlue3','LightSteelBlue2'\
          'aquamarine4','aquamarine2','burlywood4','burlywood3','SlateBlue4','SlateBlue3'\
          'dark slate blue','medium slate blue','SpringGreen4','SpringGreen3','thistle3','thistle1'\
          'RosyBrown3','RosyBrown1','purple4','purple3','LightBlue4','LightBlue3',\
          'grey42','grey54','khaki4','khaki3','DeepPink4','DeepPink3']
global boardWidth 
boardWidth = 50
global boardHeight
boardHeight = 50

class snekGUI:
    def __init__(self, width, height, server):
        global boardWidth
	global boardHeight
        self.server = server
        self.root = Tk()
        self.root.title("JustSnekThings")
        self.canvas = Canvas(self.root, width=boardWidth*width, height=boardHeight*height)
        self.root.bind('<Left>', self.left)
        self.root.bind('<Right>', self.right)
        self.root.bind('<Up>', self.up)
        self.root.bind('<Down>', self.down)
        self.root.bind('q', self.quit)
	self.canvas.pack()
	
        self.root.after(0, self.get_board)
        self.root.mainloop()

    def left(self, _):
        #print "moved left"
        cast(self.server, Atom("left"))
    def right(self, _):
        #print "moved right"
        cast(self.server, Atom("right"))
    def up(self, _):
        #print "moved up"
        cast(self.server, Atom("up"))
    def down(self, _):
        #print "moved down"
        cast(self.server, Atom("down"))
    def quit(self, _):
        print "quit"
        cast(self.server, Atom("quit"))

    def get_board(self):
        board = call(Atom("just_snek_things"), Atom("get_board"), [self.server])
        self.print_board(board)
        self.root.after(50, self.get_board)

    def print_board(self, board):
        global tokenlist
        global colors

        #print "got to print board"

        for row in range(len(board)):
            for col in range(len(board[row])):
                #print "got in loop"
                pix = board[row][col]
                color = tokenlist.index(pix)
                self.canvas.create_rectangle(col*10, row*10, (col+1)*10, (row+1)*10, fill=colors[color])
