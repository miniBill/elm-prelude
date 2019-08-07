{-# LANGUAGE PackageImports #-}

module Dict
  ( Dict
  , empty
  , get
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as DMS
import           Kernel          (Comparable (..), Equatable (..),
                                  orderToOrdering)
import           Prelude
import qualified "base" Prelude  as P

newtype Dict k v =
  Dict (Map (Comp k) v)

newtype Comp v =
  Comp v

instance Equatable v => P.Eq (Comp v) where
  (Comp l) == (Comp r) = equal l r

instance Comparable v => P.Ord (Comp v) where
  compare (Comp l) (Comp r) = orderToOrdering $ Kernel.compare l r

get :: Comparable k => k -> Dict k v -> Maybe v
get k (Dict m) = DMS.lookup (Comp k) m

empty :: Dict k v
empty = Dict DMS.empty
