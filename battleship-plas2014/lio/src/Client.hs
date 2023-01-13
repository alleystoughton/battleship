{-# LANGUAGE Safe #-}

{-|

Module      : Client
Description : (Untrusted) Client Side of Player.

This untrusted module implements the client side of a player, which
communicates with its server side via TCP.

-}

module Client(client) where

import LIO
import LIO.DCLabel

import Sys
import qualified Command

client :: Handle -> Handle -> Handle -> DC()

-- ^ @'client' stdin stdout serv@ returns a 'DC' action that, when run
-- starts up the client side of a player, using \"resign\" as the
-- quit message (see "Command").
--
-- * @stdin@ and @stdout@ are the standard input and output for communicating
-- with the user's terminal; they should have label 'dcPublic'.
--
-- * @serv@ is the handle for communicating with the server side of the
-- player; it should have label 'dcPublic'.
--
-- * The 'DC' action should be run with current label 'dcPublic', and
-- it maintains this current label throughout its execution.

client stdin stdout serv = do
  hPutStrLn stdout "connected"
  Command.clientLoop stdin stdout serv "resign"
