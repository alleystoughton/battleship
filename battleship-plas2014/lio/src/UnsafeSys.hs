{-# LANGUAGE Unsafe #-}

{-|

Module      : UnsafeSys
Description : (Trusted, Unsafe) System Functions and Types.

This trusted module is Unsafe, and so may only be used by Unsafe or
Trustworthy modules.  It provides a function for creating a privilege
from a principal, exports the 'ExitCode' type and constructors,
defines functions for specializing the standard input and output
handles to a given label, exports several network types and
constructors, defines the LIO versions of some network functions, and
defines a function for running a 'DC'-based main function.

-}

module UnsafeSys
       (-- * Creating Privilege from Principal
        privOfPrin,
        -- * Exit Codes (from "System.Exit")
        ExitCode(..),
        -- * Labeling Standard Input and Output
        labelStdinP, labelStdoutP,
        -- * TCP Types and Functions (from "Network")
        HostName,
        PortID(..),
        PortNumber,
        Socket,
        sClose, sCloseP,
        listenOnP,
        acceptP,
        connectToP,
        -- * Running a Program
        runProg)
       where

import qualified System.IO as IO
import System.Exit
import qualified System.Posix as Posix
import qualified System.Environment as SE
import Network (HostName, PortID(..), PortNumber)
import qualified Network as Net

import LIO
import LIO.DCLabel
import LIO.Concurrent
import LIO.TCB
import LIO.TCB.LObj

import Sys

privOfPrin :: Principal -> DCPriv

-- ^ @'privOfPrin' prin@ produces the privilege @priv@ such that
-- @'toCNF' priv == 'toCNF' prin@.

privOfPrin prin = PrivTCB $ toCNF prin

-- standard input and output handles

labelStdinP :: DCPriv -> DCLabel -> DC Handle

-- ^ Returns ('System.IO.stdin') labeled with the supplied label.
-- Raises an exception if -- modulo the supplied privilege -- this
-- label isn't greater than or equal to the current label, or
-- if this label isn't less than or equal to the current clearance.

labelStdinP priv l = do
  guardAllocP priv l
  return $ LObjTCB l IO.stdin

labelStdoutP :: DCPriv -> DCLabel -> DC Handle

-- ^ Returns ('System.IO.stdout') labeled with the supplied label.
-- Raises an exception if -- modulo the supplied privilege -- this
-- label isn't greater than or equal to the current label, or if
-- this label isn't less than or equal to the current clearance.

labelStdoutP priv l = do
  guardAllocP priv l
  return $ LObjTCB l IO.stdout

-- | TCP sockets as labeled objects.

type Socket = LObj DCLabel Net.Socket

-- wrappers for closing a socket

sClose :: Socket -> DC()

-- ^ The LIO analogue of 'Network.sClose'.  Will raise the current
-- label to the sockets's label; raises an exception if this isn't possible.

sClose = blessTCB "sClose" Net.sClose

sCloseP :: DCPriv -> Socket -> DC()

-- ^ The version of 'sClose' that uses a privilege to limit the
-- raising of the current label: the goal is for the current label and
-- the socket's label to be equal, modulo the privilege.

sCloseP = blessPTCB "sCloseP" Net.sClose

listenOnP :: DCPriv -> DCLabel -> PortNumber -> DC Socket

-- ^ The LIO analogue of 'Network.listenOn', returning a socket whose
-- label is the supplied label.  Raises an exception when the port
-- can't be listened on, or when -- modulo the supplied privilege --
-- the supplied label isn't greater than or equal to the current
-- label, or when the supplied label isn't less than or equal to
-- the current clearance.

listenOnP priv l port = do
  guardAllocP priv l
  sock <- ioTCB $ Net.listenOn $ PortNumber port
  return(LObjTCB l sock)

acceptP :: DCPriv -> DCLabel -> Socket -> DC Handle

-- ^ The LIO analogue of 'Network.accept', returning a handle with the
-- supplied label.  The handle's buffering mode is set to line
-- buffering.  If necessary -- given the supplied privilege -- will
-- raise the current label to the socket's label; raises an exception
-- if this isn't possible. Raises an exception if -- modulo the
-- supplied privilege -- the supplied label isn't greater than or
-- equal to the new current label, or if the supplied label isn't less
-- than or equal to the current clearance.

acceptP priv lab s = do
  (ioh, _, _) <- blessPTCB "acceptP" Net.accept priv s
  ioTCB $ IO.hSetBuffering ioh IO.LineBuffering
  guardAllocP priv lab
  return $ LObjTCB lab ioh

connectToP :: DCPriv -> DCLabel -> HostName -> PortNumber -> DC Handle

-- ^ The LIO analogue of 'Network.connectTo', returning a handle with
-- the supplied label.  The handle's buffering mode is set to line
-- buffering. Raises an exception if the supplied label -- modulo the
-- supplied privilege -- isn't greater than or equal to the current
-- label, or if the supplied label isn't less than or equal to
-- the current clearance.

connectToP priv lab host port = do
  ioh <- ioTCB $ Net.connectTo host (PortNumber port)
  ioTCB $ IO.hSetBuffering ioh IO.LineBuffering
  guardAllocP priv lab
  return $ LObjTCB lab ioh

runProg :: (String -> [String] -> DC ExitCode) -> IO()

-- ^ Turn a 'DC' main function, @main@, into an 'IO' action that, when run,
-- will:
--
--   * make the standard output be line-buffered;
-- 
--   * cause @PIPE@ signals to be ignored (so that writing to a closed TCP
--   connection doesn't result in program termination);
--
--   * call @main@ with the name by which the program was invoked and its
--   command line arguments, and run the resulting 'DC' action with
--   label 'dcPublic' and clearance @'cFalse' %% 'cTrue'@;
--
--   * exit with the exit status yielded by running the 'DC' action.

runProg m = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  Posix.installHandler Posix.sigPIPE Posix.Ignore Nothing
  prog <- SE.getProgName
  args <- SE.getArgs
  stat <- evalDC $ m prog args
  exitWith stat  
