from Tkinter import *
from erlport.erlterms import Atom
from erlport.erlang import cast
from erlport.erlang import call
import threading

class snekGUI:
    colors = {
        ' ': "white", '*': "spring green", '@': "lawn green",
        'A': "maroon2", 'a': "maroon3"
        #'B': , 'b': , 'C': , 'c': , 'D': , 'd': ,
        #'E': , 'e': , 'F': , 'f': , 'G': , 'g':
    }
    def __init__(self, width, height, server):
        self.server = server
        self.root = Tk()
        self.root.title("JustSnekThings")
        self.canvas = Canvas(self.root, width=10*width, height=10*height)
        self.root.bind('<Left>', self.left)
        self.root.bind('<Right>', self.right)
        self.root.bind('<Up>', self.up)
        self.root.bind('<Down>', self.down)
        self.root.bind('q', self.quit)
        self.canvas.pack()

        self.root.after(0, self.get_board)
        self.root.mainloop()

    def left(self, _):
        print "moved left"
        cast(self.server, Atom("left"))
    def right(self, _):
        print "moved right"
        cast(self.server, Atom("right"))
    def up(self, _):
        print "moved up"
        cast(self.server, Atom("up"))
    def down(self, _):
        print "moved down"
        cast(self.server, Atom("down"))
    def quit(self, _):
        print "quit"
        cast(self.server, Atom("quit"))

    def get_board(self):
        board = call(Atom("just_snek_things"), Atom("get_board"), [self.server])
        self.print_board(board)
        self.root.after(50, self.get_board)

    def print_board(self, board):
        print "got to print board"

        for row in range(board):
            for col in range(board[row]):
                print "got in loop"
                pix = board[row][col]
                self.canvas.create_rectangle(col*10, row*10, (col+1)*10,
                                             (row+1)*10, fill=colors[pix])
