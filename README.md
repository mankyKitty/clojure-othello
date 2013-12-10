# Othello (Clojure Stylez)

This is a Clojure implementation of the incredibly awesome Othello
board game. I plan on making this into a ClojureScript project, as
well as maybe a desktop OpenGL version just as an excuse to play with
OpenGL because...it's OpenGL.

## Installation

Nothing to install yet though feel free to take a fork full and toy
with the code. I will update this as much as I can.

## Usage

Something like ```lein trampoline run``` should have you up and
running. This assumes you have ```leiningen``` installed and all
happy.

The game will start with the black player making the first move and
the on it goes. The input for the turns as the game will tell you is
ROW then COLUMN. So ```3d```, for example.

Basically nearly anything including giving it a stern look will make
it crap itself, so be gentle and enjoy.

Also the moves algorithms are still kinda broken in some
circumstances, so don't be surprised if things go a little wonky.

In fact there are a lot of things missing but hey, it's Othello and
Clojure so it's awesome be association. \o/

## Options

Initial Options I might put in a conf file of some description.

## Examples

Current output for an 8x8 board, isn't it PRETTY!
```
Scores !
Black : 2
White : 2
    A   B   C   D   E   F   G   H
 1|   |   |   |   |   |   |   |   |
 2|   |   |   |   |   |   |   |   |
 3|   |   |   |   |   |   |   |   |
 4|   |   |   | W | B |   |   |   |
 5|   |   |   | B | W |   |   |   |
 6|   |   |   |   |   |   |   |   |
 7|   |   |   |   |   |   |   |   |
 8|   |   |   |   |   |   |   |   |
Black Player's Turn!
Enter Row and Column Number (eg. 3d) or Q/q to quit:
```
### Bugs

*cough*__Lots__*cough*

## License

Copyright © 2013 Sean Chalmers

Distributed under the Eclipse Public License, the same as Clojure.
