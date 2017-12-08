"""

frontend.py

Tufts Comp 50CP - Group Project

Team: JustSnekThings
Members: Anne Oursler, Lexi Galantino, Matt Turner

"""


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
      'maroon2','maroon3','OliveDrab4','OliveDrab1','MediumPurple4',\
      'MediumPurple1', 'deep sky blue','sky blue','dark orchid',\
      'medium orchid','gold4','gold3', 'PaleTurquoise4','PaleTurquoise3', \
      'blue4','blue2','maroon','pale violet red', 'yellow','light yellow',\
      'sienna4','sienna3','cyan','turquoise', 'dark orange', 'orange',\
      'seashell4','seashell3','LightSteelBlue3','LightSteelBlue2',\
      'aquamarine4','aquamarine2','burlywood4','burlywood3','SlateBlue4',\
      'SlateBlue3','dark slate blue','medium slate blue','SpringGreen4',\
      'SpringGreen3','thistle3','thistle1', 'RosyBrown3','RosyBrown1',\
      'purple4','purple3','LightBlue4','LightBlue3',\
      'grey42','grey54','khaki4','khaki3','DeepPink4','DeepPink3']
      
boardWidth = 50
boardHeight = 50

# GUI class which instantiates a tkinter instance and loops it
class snekGUI:
    """
    snekGUI

        A python class to act as the functions and state for a frontent
        GUI client for the snek system. Implementation done with erlport
        and Tkinter
    """

    def __init__(self, width, height, server):
        """
        __init__(self, width, height, server):
            
            The instatiation protocol for the snekGUI class. Does all
            necessary bookkeeping for initial GUI state, and enters a 
            loop to receive and stream data across the system, connecting
            client and server.

            Inputs:     width:  A server passed paramater of board width
                        height: A server passed paramter of board height 
                        server: The calling erlang server information
            Outputs:    Upon completion, opens GUI annd enters a receive loop
        """

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



    def left(self, _):
        """
        def left(self, _):
            
            Tkinter keymapped function to digest left input

            Inputs:     keypress
            Outputs:    erlang message of keypress
        """

        cast(self.server, Atom("left"))


    def right(self, _):
        """
        def right(self, _):
            
            Tkinter keymapped function to digest right input

            Inputs:     keypress
            Outputs:    erlang message of keypress
        """

        cast(self.server, Atom("right"))


    def up(self, _):
        """
        def up(self, _):
            
            Tkinter keymapped function to digest up input

            Inputs:     keypress
            Outputs:    erlang message of keypress
        """

        cast(self.server, Atom("up"))
    

    def down(self, _):
        """
        def down(self, _):
            
            Tkinter keymapped function to digest down input

            Inputs:     keypress
            Outputs:    erlang message of keypress
        """

        cast(self.server, Atom("down"))



    def quit(self, _):
        """
        def quit(self, _):
            
            Tkinter keymapped function to digest q input as quit

            Inputs:     keypress
            Outputs:    erlang message of keypress, then GUI exit
        """

        cast(self.server, Atom("quit"))
        time.sleep(1)
        self.root.destroy()
        sys.exit(0)

    
    def get_board(self):
        """
        get_board(self)::

            Internal function to request board from erlang client

            Outputs:    An internal call to print the received board.
                        Also method of passing death request from erlang
                        server
        """

        board = call(Atom("just_snek_things"), Atom("get_board"), \
            [self.server])
        if board == Atom("die"):
            time.sleep(1)
            self.root.destroy()
            sys.exit(0)            
        
        self.print_board(board)
        self.root.after(200, self.get_board)

    
    def print_board(self, board):
        """
        print_board(self, board):

            Internal function to print the input board to the GUI canvas

            Inputs:     A tuple passed via erlang of the most current board
            Outputs:    An updated GUI state representing curret board state
        """

        global tokenlist
        global colors

        for row in range(len(board)):
            for col in range(len(board[row])):
                pix = board[row][col]
                color = tokenlist.index(pix)
                self.canvas.create_rectangle(col*15, row*15, (col+1)*15, \
                 (row+1)*15, fill=colors[color])
