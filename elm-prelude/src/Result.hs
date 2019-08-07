module Result
  ( Result(..)
  ) where

data Result err ok
  = Err err
  | Ok ok
