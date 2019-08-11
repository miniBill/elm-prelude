module CLI.Types.Internal
  ( Cmd(..)
  , Focus(..)
  , Sub(..)
  ) where

data Focus
  = ChildFocus Int Focus
  | This Int

newtype Cmd msg =
  Cmd
    { runCmd :: List (IO msg)
    }

newtype Sub msg =
  Sub
    { runSub :: ()
    }
