module Cmd
  ( Cmd
  , batch
  , none
  ) where

import           CLI.Types.Internal (Cmd (..))
import qualified List

batch :: List (Cmd msg) -> Cmd msg
batch = Cmd . List.concatMap runCmd

none :: Cmd msg
none = batch []
