from Tkinter import *

class Board:
    def __init__(self, height, width):
        self.root = Tk()
        self.root.title("JustSnekThings")
        self.canvas = Canvas(self.root, width=width, height=height)
        self.root.bind('<Left>', self.left)
        self.root.bind('<Right>', self.right)
        self.root.bind('<Up>', self.up)
        self.root.bind('<Down>', self.down)
        self.root.bind('q', self.quit)
        self.canvas.pack()

        self.root.mainloop()

    def left(_, __):
        print "moved left"
    def right(_, __):
        print "moved right"
    def up(_, __):
        print "moved up"
    def down(_, __):
        print "moved down"
    def quit(_, __):
        print "quit"

myboard = Board(500, 700)
