module Utils.Graph where

import Prelude hiding (reverse)
import qualified Data.Map as Map


reverse :: (Ord a) => Map.Map a [a] -> Map.Map a [a]
reverse graph =
    Map.foldrWithKey flipEdges Map.empty graph
  where
    flipEdges name dependencies reversedGraph =
        foldr (insertDependency name) reversedGraph dependencies

    insertDependency name dep reversedGraph =
        Map.insertWith (++) dep [name] reversedGraph