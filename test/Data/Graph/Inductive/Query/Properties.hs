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

import Test.QuickCheck

import Data.List (sort)

-- -----------------------------------------------------------------------------
-- Articulation Points

-- TODO

-- -----------------------------------------------------------------------------
-- BCC

-- | Test that the bi-connected components are indeed composed solely
--   from the original graph (and comprise the entire original graph).
test_bcc :: (ArbGraph gr, Ord a, Ord b) => Proxy (gr a b) -> UConnected gr a b -> Property
test_bcc _ cg = not (isEmpty g) ==> sort (concatMap labEdges bgs) == sort (labEdges g)
                                    -- Don't test labNodes as a node
                                    -- may be repeated in multiple
                                    -- bi-connected components.
  where
    g = connGraph cg

    bgs = bcc g


-- -----------------------------------------------------------------------------
-- Utility functions

type UConnected gr a b = Connected (Undirected gr) a b
