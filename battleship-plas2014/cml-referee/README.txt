(c) MIT Lincoln Laboratory 2014

This directory contains an implementation of Battleship in Concurrent
ML that uses a trusted referee.

Players are untrusted, EXCEPT that one must manually verify that they
only communicate via their interfaces.

The program uses a client/server architecture.  Almost all of the
computation happens in the server, which spawns two players as
threads.  A client communicates with the server via a TCP connection,
connecting a player's client and server sides.

----

You will need to install the Standard ML of New Jersey (SML/NJ)
compiler yourself.  See smlnj.org for instructions.

To compile the program, simply run

./build

It puts a heap image for battleship in the bin directory.

Edit the script bin/battleship so that the variable sml is set to
the full pathname of the sml executable (by default, /usr/local/bin/sml),
and the variable dist is set to the full pathname of the cml-referee
directory.

The program can be run as follows.

First, start the server running (optionally replace 2345 by a
different port):

bin/battleship server 2345

Then, each of the two players can start (replace localhost by the
hostname of the server, if needed) battleship:

bin/battleship client localhost 2345

Usage should be self-explanatory.

----

The SML source is in the subdirectory src.

See the Compilation Manager (CM) file battleship.cm for the
program's organization.
