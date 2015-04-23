{- |
   Module      : Data.Graph.Inductive.Query.Properties
   Description : Properties for Query modules
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com

Rather than having an individual module of properties for each
`Data.Graph.Inductive.Query.*` module, this combines all such
properties and tests into one module.

 -}
module Data.Graph.Inductive.Query.Properties where

import Data.Graph.Inductive.Arbitrary
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Proxy
import Data.Graph.Inductive.Query

import Test.QuickCheck (Arbitrary (..), Gen)

import Data.List (sort)

-- -----------------------------------------------------------------------------
-- Articulation Points

-- TODO

-- -----------------------------------------------------------------------------
-- BCC

test_bcc :: (DynGraph gr, Eq b) => Proxy (gr a b) -> Bool
test_bcc p = sort (concatMap edges $ bcc g) == sort (edges g)
  where
    g = mkUndirectedUGraph [1..5] [(1, 2), (2, 3), (2, 4), (3, 5)] `asProxyGraphTypeOf` p

bccEdges g = sort (concat $ map edges $ bcc g) == sort (edges g)

-- -----------------------------------------------------------------------------
-- Utility functions

genConnected :: (DynGraph gr, Arbitrary a, Arbitrary b) => Gen (gr a b)
genConnected = do GNEs ns es <- arbitrary
                  let g = mkGraph ns es
                  let [v] = newNodes 1 g
                  return g

-- | Create an undirected, unlabelled graph.
mkUndirectedUGraph :: (Graph gr) => [Node] -> [Edge] -> gr () ()
mkUndirectedUGraph v e = mkUGraph v [ (x, y) | (x0, y0) <- e, (x, y) <- [(x0, y0), (y0, x0)] ]
