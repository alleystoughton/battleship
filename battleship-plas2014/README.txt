Battleship Case Study

(c) MIT Lincoln Laboratory 2014

Author: Alley Stoughton
Date:   June 17, 2014

This directory contains the program code for the case study in secure
programming described in the paper _You Sank My Battleship! A Case
Study in Secure Programming_, by Alley Stoughton, Andrew Johnson,
Samuel Beller, Karishma Chadha, Dennis Chen, Kenneth Foner and Michael
Zhivich, presented at the ACM Ninth Workshop on Programming Languages
and Analysis for Security (PLAS 2014), and appearing in the ACM
digital library.

The case study consists of three implementations of the game
Battleship: one in Concurrent ML using a trusted referee, one in
Haskell/LIO using information flow control to avoid the need for a
trusted referee, and one in Concurrent ML using access control to
avoid needing a trusted referee.

Subdirectories:

  cml-referee

    an implementation in Concurrent ML using a trusted referee

  lio

    an implementation in Haskell/LIO using information flow control to
    avoid the need for a trusted referee

  cml-ac

    an implementation in CML using access control (implemented using
    unforgeable keys plus data abstraction) to avoid the need for a
    trusted referee
