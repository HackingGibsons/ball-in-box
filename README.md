# Ball-in-a-Box
The game

## The idea
A ball bouncing around the world trying to accelerate forever to gain the most
points in the crazy thing we call a world while staying in as big of a peice
as possible.

## The project
It's a CommonLisp project powered by SBCL using Lipbuilders-SDL and OpenGL

## The.. what now?
We wanted to build a game, and we kinda like lisp.
You can try it at home:
    $ git clone https://sshirokov@github.com/HackingGibsons/ball-in-box.git
    $ cd ball-in-box
    $ make develop
    $ sbcl --eval '(ql:quickload :ball-in-box)' \
           --eval '(ball-in-box:ball-in-box)' \
           --eval '(quit)'


