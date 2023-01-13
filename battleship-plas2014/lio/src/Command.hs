{-# LANGUAGE Safe #-}

{-|

Module      : Command
Description : (Untrusted) Command-oriented Processing via Internet Connection.

This untrusted module implements the protocol by which the client and
server sides of a player communicate, over a TCP connection.

The handles supplied to the following functions should be labeled
'dcPublic'.  The 'DC' actions returned by the functions should be
invoked with current label 'dcPublic', and they maintain the current
label at 'dcPublic' throughout their executions.

-}

module Command
       (-- * Server Functions
        doneServer,
        infoServer,
        promptServer,
        -- * Client Function
        clientLoop)
       where

import LIO
import LIO.DCLabel
import Control.Monad

import Aux
import Sys

-- auxiliary functions

sendLineToHandle :: Handle -> String -> DC()

sendLineToHandle h x =
  catch
  (hPutStrLn h x)
  ((\ _ -> return()) :: SomeException -> DC())
    -- if handle closed at other side, ignore exception

recvLineFromHandle :: Handle -> DC(Maybe String)

-- trailing newline is stripped off; returns Nothing on end-of-file
-- or if the other side of a TCP connection has closed the connection

recvLineFromHandle h =
  catch
  (hGetLine h >>= return . Just)
  ((\ _ -> return Nothing) :: SomeException -> DC(Maybe String))

-- messages sent from server to client

data ServerMsg = DoneServerMsg
               | InfoServerMsg String
               | PromptServerMsg String

-- auxiliary function used by server

sendServerMsgToHandle :: Handle -> ServerMsg -> DC()

sendServerMsgToHandle clnt msg =
  case msg of
    DoneServerMsg     -> sendLineToHandle clnt "."
    InfoServerMsg x   -> sendLineToHandle clnt (">" ++ x)
    PromptServerMsg x -> sendLineToHandle clnt ("<" ++ x)

-- auxiliary function used by the client loop

recvServerMsgFromHandle :: Handle -> DC ServerMsg

recvServerMsgFromHandle serv = do
  sm <- recvLineFromHandle serv
  case sm of
    Nothing  -> return DoneServerMsg  -- server closed connection
    Just "." -> return DoneServerMsg
    Just x   ->
      case head x of
        '>' -> return $ InfoServerMsg $ tail x
        '<' -> return $ PromptServerMsg $ tail x
        _   -> return $ DoneServerMsg  -- shouldn't happen

-- server functions

doneServer :: Handle -> DC()

-- ^ @'doneServer' clnt@ returns a 'DC' action that, when run, tells the client
-- (via @clnt@) to exit.

doneServer clnt = sendServerMsgToHandle clnt DoneServerMsg

infoServer :: Handle -> [String] -> DC()

-- ^ @'infoServer' clnt msgs@ returns a 'DC' action that, when run, tells the
-- client (via @clnt@) to display the elements of @msgs@ as separate lines,
-- in order.

infoServer clnt msgs =
  forM_ msgs
  (\ msg -> sendServerMsgToHandle clnt $ InfoServerMsg msg)

promptServer :: Handle -> String -> String -> DC(Maybe String)

-- ^ @'promptServer' clnt prompt quitMsg@ returns a 'DC' action that,
-- when run, tells the client (via @clnt@) to prompt its user for a line
-- of input, using @prompt@, and then send this input -- stripped of
-- leading/trailing whitespace -- to the server, so it may be
-- returned, decorated by 'Just', as the result of the 'DC' action.
--
-- @quitMsg@ (which should not have leading or trailing whitespace)
-- should be the same /quit message/ passed to 'clientLoop'. If the
-- client's user signals end-of-file, or enters @quitMsg@ (optionally
-- surrounded by whitespace) as its line, then the client will send
-- @quitMsg@ to the server, causing the 'DC' action to return
-- 'Nothing'.  If the client's user interrupts (usually CTRL-c) (or
-- has already interrupted at the point when 'promptServer' is
-- called), then the 'DC' action will also return 'Nothing'.

promptServer clnt prompt quitMsg = do
  sendServerMsgToHandle clnt $ PromptServerMsg prompt
  x <- recvLineFromHandle clnt
  case x of
    Nothing -> return Nothing
    Just x  -> if x == quitMsg then return Nothing else return $ Just x

-- client function

clientLoop :: Handle -> Handle -> Handle -> String -> DC()

-- ^ @'clientLoop' stdin stdout serv quitMsg@ returns a 'DC' action
-- that, when run, runs the /client loop/, communicating with the user
-- via @stdin@/@stdout@, and with the server via @serv@. It uses
-- @quitMsg@ (see 'promptServer') as the quit message. Upon
-- termination, the 'DC' action returns @()@.

clientLoop stdin stdout serv quitMsg = do
  msg <- recvServerMsgFromHandle serv
  case msg of
    DoneServerMsg     -> return()
    InfoServerMsg x   -> do
      hPutStrLn stdout x
      clientLoop stdin stdout serv quitMsg
    PromptServerMsg x -> do
      hPutStrLn stdout x
      yMay <- recvLineFromHandle stdin
      case yMay of
        Nothing -> do -- end of file
          sendLineToHandle serv quitMsg
          clientLoop stdin stdout serv quitMsg
        Just y  -> do
          let z = Aux.strip y
          sendLineToHandle serv z
          clientLoop stdin stdout serv quitMsg
