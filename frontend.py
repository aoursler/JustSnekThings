from Tkinter import *
from erlport.erlterms import Atom
from erlport.erlang import cast

class snekGUI:
    def __init__(self, height, width, server):
        self.server = server
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
    def quit(_, __):
        print "quit"
        # call(self.server, Atom("unsubscribe"), [GameName, HostName,
        #                                                  UserName])
