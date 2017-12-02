from Tkinter import *
#from erlport.erlterms import Atom
#from erlport.erlang import call

class snekGUI:
    def __init__(self, height, width, GameName, HostName, UserName):
        self.GameName, self.HostName, self.UserName = GameName, HostName, UserName
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
        # call(Atom("pyserver"), Atom("move"), [GameName, HostName, UserName,
        #                                         Atom("left")])
    def right(_, __):
        print "moved right"
        # call(Atom("pyserver"), Atom("move"), [GameName, HostName, UserName,
        #                                         Atom("right")])
    def up(_, __):
        print "moved up"
        # call(Atom("pyserver"), Atom("move"), [GameName, HostName, UserName,
        #                                         Atom("up")])
    def down(_, __):
        print "moved down"
        # call(Atom("pyserver"), Atom("move"), [GameName, HostName, UserName,
        #                                         Atom("down")])
    def quit(_, __):
        print "quit"
        # call(Atom("pyserver"), Atom("unsubscribe"), [GameName, HostName,
        #                                                 UserName])

myboard = snekGUI(500, 700, "game", "host", "username")
