module Util (asManyAs, reorderBy) where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List     (sortBy)

reorderBy :: Ord b => [a] -> [b] -> [a]
reorderBy xs = fmap fst . sortBy (compare `on` snd) . zip xs

-- length returns an Int that is fed into replicateM, creating a new function
-- that no longer requires an Int as first argument. It is only possible because
-- the return value of length is Int, which is also the first parameter required
-- by replicateM. The result is a function that requires t a (for length) and
-- m a (for replicateM)
asManyAs :: (Foldable t, Applicative m) => t a -> m b -> m [b]
asManyAs = replicateM . length
