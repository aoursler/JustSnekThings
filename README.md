Just Snek Things

A distributed game of Snake that allows players on the same network to
play together.


Contributers

This project was writen by Anne Oursler, Lexi Galantino, and Matt Turner
for their Tufts Concurrent Programming Final Project.


Files:

flow.txt: Example commands to start and join a game of snek, after one
          has cloned the snek directory and installed the dependencies. 

frontend.py: GUI logic for snek related games. The GUI is written in
             python and uses Tkinter.

just_snek_things.erl: Server to allows multiple players to play snek on
                      the same server.

Python files with game logic for various snek games, to be invoked and
instantiated by a calling erlang server upon creation of a python VM on
an attached erlang node:

snek.py: A generic game of snake where you start with 50 energy and use
         powerups to sustain yourself. Your tail will grow no longer than
         20 pixels.

infsnek.py: Snek but with no limit on tail length.

kingsnek.py: Snek where player play to a high score of 1,000, and respawn
             on death with their previous body turning into powerups.

infkingsnek.py: KingSnek where there is not limit on tail length.

votesnek.py: A 'Lets Play' version of Snek where users vote together to
             move the snek every 20 votes.
