{-# LANGUAGE Unsafe #-}

{-|

Module      : Sys
Description : (Trusted, Unsafe) Main Processing.

This trusted module defines the program's main function.  It's Unsafe, and so
may not be used by Safe modules.

-}

-- main processing

module Main(main) where

import Data.Monoid

import LIO
import LIO.DCLabel
import LIO.Concurrent

import Sys
import UnsafeSys
import qualified Aux
import qualified Command
import qualified PlayerInterfaceMsg as PIM
import qualified PlayerInterface as PI
import qualified Client as C

-- error message

err :: String -> DC ExitCode
 
err msg = do
  stdout <- labelStdoutP mempty dcPublic
  hPutStrLn stdout msg
  return $ ExitFailure 1

-- running a DC action, and then issuing fatal error if exception
-- raised or current label or clearance changed  

catchFatlPI :: DC a -> DC a

catchFatlPI =
  Sys.catchFatal
  "main"
  "player interface raised exception"
  "won't happen"

catchFatlClient :: DC a -> DC a

catchFatlClient =
  Sys.catchFatal
  "main"
  "client raised exception"
  "client modified current label or clearance"

referee :: Handle -> Handle -> DC()

-- the referee

referee clnt1 clnt2 = do
  -- we need two message pairs, since sending/receiving isn't synchronous
  (send1, recv1) <- PIM.piMsgPair
  (send2, recv2) <- PIM.piMsgPair
  lr1 <-
    lFork dcPublic $
    PI.playerInterface clnt1 send1 recv2 True
    (privOfPrin $ principal "player1")
  lr2 <-
    lFork dcPublic $
    PI.playerInterface clnt2 send2 recv1 False
    (privOfPrin $ principal "player2")
  _ <- catchFatlPI $ lWait lr1
  _ <- catchFatlPI $ lWait lr2
  return()

server :: PortNumber -> DC ExitCode

-- the server

server port = do
  -- clearance will be False %% True, and label will be dcPublic
  loRes <-
    try(listenOnP mempty dcPublic port) :: DC(Either SomeException Socket)
  case loRes of
    Left _     -> err "port invalid or already in use"
    Right sock -> do
      stdout <- labelStdoutP mempty dcPublic
      hPutStrLn stdout ("listening on port " ++ show port ++ " ...")
      clnt1 <- acceptP mempty dcPublic sock
      hPutStrLn stdout "Player 1 connected"
      clnt2 <- acceptP mempty dcPublic sock
      hPutStrLn stdout "Player 2 connected"
      referee clnt1 clnt2
      hPutStrLn stdout "shutting down"
      return ExitSuccess

-- client side of player

client :: String -> PortNumber -> DC ExitCode

client host port = do
  -- clearance will be False %% True, and label will be dcPublic
  ctRes <-
    try(connectToP mempty dcPublic host port) ::
      DC(Either SomeException Handle)
  case ctRes of
    Left _     -> err "cannot connect to port on host"
    Right serv -> do
      stdin <- labelStdinP mempty dcPublic
      stdout <- labelStdoutP mempty dcPublic
      catchFatlClient $ C.client stdin stdout serv
      return ExitSuccess

-- entry point

usage :: String -> DC ExitCode

-- issue usage message, and return with failure exit code

usage prog = do
  stdout <- labelStdoutP mempty dcPublic
  hPutStrLn stdout $
      "usage:\n" ++
    "  " ++ prog ++ " server PORT\n" ++
    "  " ++ prog ++ " client HOST PORT"
  return $ ExitFailure 1

main :: IO()

-- ^ Program's entry point.  When 'main' is run, it matches the program's
-- command line arguments against the following patterns, issuing
-- an error message if pattern matching fails:
--
-- * @[\"server\", port]@
--   Starts the server running on the specified port.
--
-- * @[\"client\", host, port]@
--   Starts an instance of the client, specifying the host and port
--   to connect to.

main = runProg $ \ prog args ->
  -- clearance will be False %% True, and label will be dcPublic
  case args of
    ["server", port]       ->
      case Aux.strToInteger port of
           Nothing   -> err "invalid port"
           Just port ->    
             if port >= 0
             then server $ fromInteger port
             else err "invalid port"
    ["client", host, port] ->
      case Aux.strToInteger port of
           Nothing   -> err "invalid port"
           Just port ->    
             if port >= 0
             then client host $ fromInteger port
             else err "invalid port"
    _                      -> usage prog
