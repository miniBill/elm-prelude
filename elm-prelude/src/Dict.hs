module Dict
  ( Dict
  , empty
  , get
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as DMS
import Kernel (Ord)

newtype Dict k v =
  Dict (Map k v)

get :: Ord k => k -> Dict k v -> Maybe v
get k (Dict m) = DMS.lookup k m

empty :: Dict k v
empty = Dict DMS.empty
