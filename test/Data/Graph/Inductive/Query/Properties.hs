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

-- | Deleting the articulation points should increase the number of
--   components.
test_ap :: (ArbGraph gr) => Proxy (gr a b) -> Undirected gr a b -> Property
test_ap _ ug = not (isEmpty g) ==>
                 null points || noComponents (delNodes points g) > noComponents g
  where
    g = toBaseGraph ug

    points = ap g

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
-- BFS

-- TODO

-- -----------------------------------------------------------------------------
-- DFS

-- TODO: flesh out

-- | The 'components' function should never return an empty list, and
--   none of its sub-lists should be empty (unless the graph is
--   empty).  All nodes in the graph should be in precisely one of the
--   components.
test_components :: (ArbGraph gr) => Proxy (gr a b) -> UConnected gr a b -> Bool
test_components _ cg = all (not . null) cs && sort (concat cs) == sort (nodes g)
  where
    g = connGraph cg

    cs = components g

-- | The strongly connected components should be a partitioning of the
--   nodes of a graph.
test_scc :: (Graph gr) => Proxy (gr a b) -> gr a b -> Bool
test_scc _ g = sort (concat (scc g)) == sort (nodes g)

-- | Every node in an undirected connected graph should be reachable.
test_reachable :: (ArbGraph gr) => Proxy (gr a b) -> UConnected gr a b -> Property
test_reachable _ cg = not (isEmpty g) ==> sort (reachable v g) == sort (nodes g)
  where
    g = connGraph cg

    v = node' . fst . matchAny $ g

-- -----------------------------------------------------------------------------
-- Utility functions

type UConnected gr a b = Connected (Undirected gr) a b
