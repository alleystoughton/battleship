{-# LANGUAGE Safe #-}

{-|

Module      : PlayerInterfaceMsg
Description : (Trusted) Communication of Messages between Player Interfaces.

This trusted module defines the datatype of messages exchanged by
player interfaces, as well as a function for creating a player
interface message pair @(send, recv)@, where @send@ is used to send a
message to the other player interface, and @recv@ is used to receive a
message from the other player interface.

-}

module PlayerInterfaceMsg
       (PIMsg(..),
        PIMsgPair,
        piMsgPair)
       where

import LIO
import LIO.Concurrent
import LIO.DCLabel

import qualified Ship
import qualified Board

-- | Messages exchanged by player interfaces.

data PIMsg =
       LBC Board.LBC -- ^ Labeled board closure.
     | Resign        -- ^ Resignation.
     | LC Board.LC   -- ^ Shot or shot response.

-- | Player interface message pair.

type PIMsgPair = (PIMsg -> DC(), DC PIMsg)

piMsgPair :: DC PIMsgPair

-- ^ A 'DC' action returning a message pair @(send, recv)@
-- manipulating a hidden 'LMVar', which is labeled 'dcPublic', and
-- starts out empty. The 'DC' action will raise an exception if
-- 'dcPublic' isn't between the current label and clearance.  The
-- current label and clearance aren't changed by the 'DC' action.
--
-- * @send msg@ returns a 'DC' action that tries to put the evaluation
-- of @msg@ into the 'LMVar', blocking until this is possible, at
-- which point @()@ is returned. The 'DC' action will raise the
-- current label to 'dcPublic'; it will raise an exception if
-- this isn't possible. The clearance won't be changed.
--
-- * @recv@ is a 'DC' action that tries to take a message from the
-- 'LMVar', blocking until this is possible, and returning the message
-- when it succeeds. The 'DC' action will raise the current label to
-- 'dcPublic'; it will raise an exception if this isn't possible. The
-- clearance won't be changed.

piMsgPair = do
  var <- newEmptyLMVar dcPublic
  return
    (\ msg -> do
        evaluate msg
        putLMVar var msg,
     takeLMVar var)
