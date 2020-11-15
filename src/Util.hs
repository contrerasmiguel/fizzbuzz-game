module Util (
      asManyAs
    , reorderBy
    ) where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List     (sortBy)

reorderBy :: Ord b => [a] -> [b] -> [a]
reorderBy xs = fmap fst . sortBy (compare `on` snd) . zip xs

asManyAs :: (Applicative m, Foldable t) => t a-> m b -> m [b]
asManyAs xs = replicateM $ length xs
