module Compat
  ( TChan
  , Either(..)
  , Monad(..)
  , MonadIO(..)
  , atomically
  , forever
  , forkIO
  , fromIntegral
  , mapM
  , mapM_
  , newTChanIO
  , orElse
  , readTChan
  , retry
  , sequence
  , writeTChan
  ) where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan,
                                               writeTChan)
import           Control.Monad                (forever, mapM, mapM_, sequence)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.STM            (atomically, orElse, retry)
import           Kernel                       (Either (..), Monad (..),
                                               fromIntegral)
