-- (c) 1999 - 2002 by Martin Erwig [see file COPYRIGHT]
-- | Tree-based implementation of 'Graph' and 'DynGraph'
--
--   You will probably have better performance using the
--   "Data.Graph.Inductive.PatriciaTree" implementation instead.

module Data.Graph.Inductive.Tree (Gr,UGr) where

import Data.Graph.Inductive.Graph

import           Control.Applicative (liftA2)
import           Control.Arrow       (first)
import           Control.DeepSeq     (NFData (..))
import           Data.List           (foldl', sort)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)

----------------------------------------------------------------------
-- GRAPH REPRESENTATION
----------------------------------------------------------------------

newtype Gr a b = Gr (GraphRep a b)

type GraphRep a b = Map Node (Context' a b)
type Context' a b = (Adj b,a,Adj b)

type UGr = Gr () ()

----------------------------------------------------------------------
-- CLASS INSTANCES
----------------------------------------------------------------------

instance (Eq a, Ord b) => Eq (Gr a b) where
  (Gr g1) == (Gr g2) = fmap sortAdj g1 == fmap sortAdj g2
    where
      sortAdj (a1,n,a2) = (sort a1,n,sort a2)

instance (Show a, Show b) => Show (Gr a b) where
  showsPrec d g = showParen (d > 10) $
                    showString "mkGraph "
                    . shows (labNodes g)
                    . showString " "
                    . shows (labEdges g)

instance (Read a, Read b) => Read (Gr a b) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("mkGraph", s) <- lex r
    (ns,t) <- reads s
    (es,u) <- reads t
    return (mkGraph ns es, u)

-- Graph
--
instance Graph Gr where
  empty             = Gr M.empty
  isEmpty (Gr g)    = M.null g
  match v gr@(Gr g) = maybe (Nothing, gr)
                            (first Just . uncurry (cleanSplit v))
                      . (\(m,g') -> fmap (flip (,) g') m)
                      $ M.updateLookupWithKey (const (const Nothing)) v g
  mkGraph vs es     = (insEdges' . insNodes vs) empty
        where
          insEdges' g = foldl' (flip insEdge) g es

  labNodes (Gr g)   = map (\(v,(_,l,_))->(v,l)) (M.toList g)
  -- more efficient versions of derived class members
  --
  matchAny (Gr g)   = maybe (error "Match Exception, Empty Graph")
                            (uncurry (uncurry cleanSplit))
                            (M.minViewWithKey g)
  noNodes   (Gr g)  = M.size g
  nodeRange (Gr g)  = fromMaybe (0,0)
                      $ liftA2 (,) (ix (M.minViewWithKey g))
                                   (ix (M.maxViewWithKey g))
    where
      ix            = fmap (fst . fst)
  labEdges  (Gr g)  = concatMap (\(v,(_,_,s))->map (\(l,w)->(v,w,l)) s) (M.toList g)

-- After a Node (with its corresponding Context') are split out of a
-- GraphRep, clean up the remainders.
cleanSplit :: Node -> Context' a b -> GraphRep a b
              -> (Context a b, Gr a b)
cleanSplit v (p,l,s) g = (c, Gr g')
  where
    -- Note: loops are kept only in successor list
    c = (p', v, l, s)
    p' = rmLoops p
    s' = rmLoops s
    rmLoops = filter ((/=v) . snd)

    g' = updAdj s' (clearPred v) . updAdj p' (clearSucc v) $ g

-- DynGraph
--
instance DynGraph Gr where
  (p,v,l,s) & (Gr g) = Gr
                       . updAdj p (addSucc v)
                       . updAdj s (addPred v)
                       $ M.alter addCntxt v g
    where
      addCntxt = maybe (Just cntxt')
                       (const (error ("Node Exception, Node: "++show v)))
      cntxt' = (p,l,s)

instance (NFData a, NFData b) => NFData (Gr a b) where
  rnf (Gr g) = rnf g

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

addSucc :: Node -> b -> Context' a b -> Context' a b
addSucc v l (p,l',s) = (p,l',(l,v):s)

addPred :: Node -> b -> Context' a b -> Context' a b
addPred v l (p,l',s) = ((l,v):p,l',s)

clearSucc :: Node -> b -> Context' a b -> Context' a b
clearSucc v _ (p,l,s) = (p,l,filter ((/=v).snd) s)

clearPred :: Node -> b -> Context' a b -> Context' a b
clearPred v _ (p,l,s) = (filter ((/=v).snd) p,l,s)

updAdj :: Adj b -> (b -> Context' a b -> Context' a b) -> GraphRep a b -> GraphRep a b
updAdj adj f g = foldl' (\g' (l,v) -> M.adjust (f l) v g') g adj
