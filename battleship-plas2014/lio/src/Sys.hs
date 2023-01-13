{-# LANGUAGE Trustworthy #-}

{-|

Module      : Sys
Description : (Trusted, Trustworthy) System Functions and Types.

This trusted module is Trustworthy, and so may be used by Safe
modules.  It defines the type 'DCMVar' of labeled, mutable variables
whose labels come from 'DCLabel'.  It also defined the type of labeled
IO/TCP handles, 'Handle'.  It defines wrappers for a number of IO
functions.  It defines functions for issuing fatal error messages and
exception handling. And it defines functions for testing if LIO guards
would succeed.

-}

module Sys
       (-- * Types
        DCMVar,
        Handle,
        -- * Wrappers for IO Functions
        hPutChar, hPutCharP,
        hPutStr, hPutStrP,
        hPutStrLn, hPutStrLnP,
        hGetChar, hGetCharP,
        hGetLine, hGetLineP,
        hSetBuffering, hSetBufferingP,
        hClose, hCloseP,
        -- * Fatal Error Messages and Exception Handling
        fatal,
        catchFatal,
        -- * Functions for Testing if LIO Guards Would Succeed
        guardAlloc_Check, guardWrite_Check,
        guardAllocP_Check, guardWriteP_Check)
       where

import qualified System.IO as IO
import System.Exit
import Control.Monad

import LIO
import LIO.DCLabel
import LIO.Concurrent
import LIO.TCB
import LIO.TCB.LObj

-- | 'LMVar' specialized to 'DCLabel'.

type DCMVar a = LMVar DCLabel a

-- | The LIO version of a IO/TCP handle: a labeled object whose value
-- has type 'System.IO.Handle'.

type Handle = LObj DCLabel IO.Handle

-- wrappers for IO functions

hPutChar :: Handle -> Char -> DC()

-- ^ The LIO analogue of 'System.IO.hPutChar'.  Will raise the current
-- label to the handle's label; raises an exception if this isn't possible.

hPutChar = blessTCB "hPutChar" IO.hPutChar

hPutCharP :: DCPriv -> Handle -> Char -> DC()

-- ^ The version of 'hPutChar' that uses a privilege to limit the
-- raising of the current label: the goal is for the current label and
-- the handle's label to be equal, modulo the privilege.

hPutCharP = blessPTCB "hPutCharP" IO.hPutChar

hPutStr :: Handle -> String -> DC()

-- ^ The LIO analogue of 'System.IO.hPutStr'.  Will raise the current
-- label to the handle's label; raises an exception if this isn't possible.

hPutStr = blessTCB "hPutStr" IO.hPutStr

hPutStrP :: DCPriv -> Handle -> String -> DC()

-- ^ The version of 'hPutStr' that uses a privilege to limit the
-- raising of the current label: the goal is for the current label and
-- the handle's label to be equal, modulo the privilege.

hPutStrP = blessPTCB "hPutStrP" IO.hPutStr

hPutStrLn :: Handle -> String -> DC()

-- ^ The LIO analogue of 'System.IO.hPutStrLn'.  Will raise the current
-- label to the handle's label; raises an exception if this isn't possible.

hPutStrLn = blessTCB "hPutStrLn" IO.hPutStrLn

hPutStrLnP :: DCPriv -> Handle -> String -> DC()

-- ^ The version of 'hPutStrLn' that uses a privilege to limit the
-- raising of the current label: the goal is for the current label and
-- the handle's label to be equal, modulo the privilege.

hPutStrLnP = blessPTCB "hPutStrLnP" IO.hPutStrLn

hGetChar :: Handle -> DC Char

-- ^ The LIO analogue of 'System.IO.hGetChar'.  Will raise the current
-- label to the handle's label; raises an exception if this isn't possible.

hGetChar = blessTCB "hGetChar" IO.hGetChar

hGetCharP :: DCPriv -> Handle -> DC Char

-- ^ The version of 'hGetChar' that uses a privilege to limit the
-- raising of the current label: the goal is for the current label and
-- the handle's label to be equal, modulo the privilege.

hGetCharP = blessPTCB "hGetCharP" IO.hGetChar

hGetLine :: Handle -> DC String

-- ^ The LIO analogue of 'System.IO.hGetLine'.  Will raise the current
-- label to the handle's label; raises an exception if this isn't possible.

hGetLine = blessTCB "hGetLine" IO.hGetLine

hGetLineP :: DCPriv -> Handle -> DC String

-- ^ The version of 'hGetLine' that uses a privilege to limit the
-- raising of the current label: the goal is for the current label and
-- the handle's label to be equal, modulo the privilege.

hGetLineP = blessPTCB "hGetLineP" IO.hGetLine

hSetBuffering :: Handle -> IO.BufferMode -> DC()

-- ^ The LIO analogue of 'System.IO.hSetBuffering'.  Will raise the current
-- label to the handle's label; raises an exception if this isn't possible.

hSetBuffering = blessTCB "hSetBuffering" IO.hSetBuffering

hSetBufferingP :: DCPriv -> Handle -> IO.BufferMode -> DC()

-- ^ The version of 'hSetBuffering' that uses a privilege to limit the
-- raising of the current label: the goal is for the current label and
-- the handle's label to be equal, modulo the privilege.

hSetBufferingP = blessPTCB "hSetBufferingP" IO.hSetBuffering

hClose :: Handle -> DC()

-- ^ The LIO analogue of 'System.IO.hClose'.  Will raise the current
-- label to the handle's label; raises an exception if this isn't possible.

hClose = blessTCB "hClose" IO.hClose

hCloseP :: DCPriv -> Handle -> DC()

-- ^ The version of 'hClose' that uses a privilege to limit the
-- raising of the current label: the goal is for the current label and
-- the handle's label to be equal, modulo the privilege.

hCloseP = blessPTCB "hCloseP" IO.hClose

fatal :: String -> String -> DC a

-- ^ @'fatal' comp msg@ returns a 'DC' action that, when run, issues
-- the error message @msg@, attributed to program component @comp@, on
-- the standard output, and then causes the program to exit with
-- failure status.

fatal comp msg = do
  ioTCB $ IO.putStrLn ("fatal error: " ++ comp ++ ": " ++ msg)
  ioTCB $ exitWith $ ExitFailure 1

catchFatal :: String -> String -> String -> DC a -> DC a

-- ^ @'catchFatal' comp msgEx msgLab m@ returns a 'DC' action that,
-- when run, saves the current label and clearance, and then runs @m@.
-- If running @m@ raises an exception, then the 'DC' action issues the
-- error message @msgEx@, attributed to program component @comp@, on
-- the standard output, and then causes the program to exit with
-- failure status. Otherwise, if running @m@ changed the current label
-- or clearance, then the 'DC' action issues the error message
-- @msgLab@, attributed to program component @comp@, on the standard
-- output, and then causes the program to exit with failure
-- status. Otherwise, the 'DC' action returns the value returned by
-- @m@.

catchFatal comp msgEx msgLab m = do
  cur <- getLabel
  clr <- getClearance
  res <-
    catch
    m
    ((\ _ -> fatal comp msgEx) :: SomeException -> DC a)
  cur' <- getLabel
  clr' <- getClearance
  if cur == cur' && clr == clr'
    then return res
    else fatal comp msgLab

guardAlloc_Check :: DCLabel -> DC Bool

-- ^ @'guardAlloc_Check' lab@ returns a 'DC' action that tests whether
-- @'guardAlloc' lab@ would succeed, were it run.

guardAlloc_Check lab = do
  cur <- getLabel
  clr <- getClearance
  return $ cur `canFlowTo` lab && lab `canFlowTo` clr

guardWrite_Check :: DCLabel -> DC Bool

-- ^ @'guardWrite_Check' lab@ returns a 'DC' action that tests whether
-- @'guardWrite' lab@ would succeed, were it run.

guardWrite_Check = guardAlloc_Check

guardAllocP_Check :: DCPriv -> DCLabel -> DC Bool

-- ^ @'guardAllocP_Check' priv lab@ returns a 'DC' action that tests whether
-- @'guardAllocP' priv lab@ would succeed, were it run.

guardAllocP_Check priv lab = do
  cur <- getLabel
  clr <- getClearance
  return $ canFlowToP priv cur lab && lab `canFlowTo` clr

guardWriteP_Check :: DCPriv -> DCLabel -> DC Bool

-- ^ @'guardWriteP_Check' priv lab@ returns a 'DC' action that tests whether
-- @'guardWriteP' priv lab@ would succeed, were it run.

guardWriteP_Check = guardAllocP_Check
