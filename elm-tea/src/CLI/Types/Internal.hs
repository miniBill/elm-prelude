module CLI.Types.Internal
  ( Focus(..)
  ) where

data Focus
  = ChildFocus Int Focus
  | This Int
