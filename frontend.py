from Tkinter import *
from erlport.erlterms import Atom
from erlport.erlang import cast
from erlport.erlang import call
from sys import *
import threading
import time

global tokenlist 
tokenlist = [' ','*','@','A','a','B','b','C','c'] 
global colors 
colors = ['white','spring green','lawn green','maroon2','maroon3','OliveDrab4','OliveDrab1','MediumPurple4','MediumPurple1']
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
        self.root.geometry('751x751')
        self.root.title("JustSnekThings")
        self.canvas = Canvas(self.root, width=boardWidth*width, height=boardHeight*height)
        self.root.bind('<Left>', self.left)
        self.root.bind('<Right>', self.right)
        self.root.bind('<Up>', self.up)
        self.root.bind('<Down>', self.down)
        self.root.bind('q', self.quit)
        self.canvas.pack()
	
        self.root.after(100, self.get_board)
        cast(self.server, Atom("link"))
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
        time.sleep(1)
        self.root.destroy()
        sys.exit(0)

    def get_board(self):
        board = call(Atom("just_snek_things"), Atom("get_board"), [self.server])
        self.print_board(board)
        self.root.after(200, self.get_board)

    def print_board(self, board):
        global tokenlist
        global colors

        #print "got to print board"

        for row in range(len(board)):
            for col in range(len(board[row])):
                #print "got in loop"
                pix = board[row][col]
                color = tokenlist.index(pix)
                self.canvas.create_rectangle(col*15, row*15, (col+1)*15, (row+1)*15, fill=colors[color])
