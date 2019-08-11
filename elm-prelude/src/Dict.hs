module Dict
  ( Dict
  , empty
  , get
  , fromList
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as DMS
import           Kernel          (Eq (..), Ord, Show (..))
import           Prelude         hiding ((/=), (==))

instance (Show k, Show v) => Show (Dict k v) where
  show (Dict m) = "fromList" ++ show (DMS.toList m)

instance (Eq k, Eq v) => Eq (Dict k v) where
  (Dict l) == (Dict r) = (l == r)

newtype Dict k v =
  Dict (Map k v)

get :: Ord k => k -> Dict k v -> Maybe v
get k (Dict m) = DMS.lookup k m

empty :: Dict k v
empty = Dict DMS.empty

fromList :: Ord k => List (k, v) -> Dict k v
fromList = Dict . DMS.fromList
