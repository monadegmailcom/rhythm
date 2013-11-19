module LengthPattern (
	Pattern(..), normalize, goDown, goUp, toTop, 
	replace, replaceLength, focus, 
	toRose, sampleTree ) where

import Rhythm

import Data.Maybe	
import Data.List (genericSplitAt, genericLength)
import Data.Tree
import Data.Tree.Pretty -- install pretty-tree and boxes

-- branch-centric implementation, branch lengths are relative to siblings
data Pattern a = Leaf a | Branch [(Length, Pattern a)]

{- normalize tree
- prune branches with length 0
- divide branch lengths by common divisor -}
normalize :: Pattern a -> Pattern a
normalize (Branch pairs) = Branch $ map f $ filter pred pairs where
		pred    = (/= 0).fst
		f (l,p) = (div l g, normalize p)
		g       = max 1 $ foldr (gcd.fst) 0 pairs		
normalize p = p

-- bread crumb, index and length of taken subtree, discarded subtrees
data Crumb a = Crumb Index Length [(Length, Pattern a)]

instance Show (Crumb a) where
	show (Crumb i l _) = show (i,l)
	
type Zipper a = (Pattern a, [Crumb a])

-- descend at subtree referenced by index	
goDown :: Index -> Zipper a -> Maybe (Zipper a)
goDown i (Branch bs, cs) 
	| i >= genericLength bs = Nothing
	| otherwise             = Just (r, (Crumb i l (ls ++ rs)):cs) where
		(ls,(l, r):rs) = genericSplitAt i bs
goDown _ _ = Nothing

-- one level up
goUp :: Zipper a -> Maybe (Zipper a)
goUp (b, (Crumb i l bs):cs) = Just ((Branch $ ls ++ (l, b):rs), cs) where
	(ls,rs) = genericSplitAt i bs
goUp _                      = Nothing

-- go back to top	
toTop :: Zipper a -> Zipper a
toTop z@(_, []) = z
toTop z         = toTop . fromJust . goUp $ z

-- follow trace
focus :: [Index] -> Pattern a -> Maybe (Zipper a)
focus is p = f is $ Just (p, []) where
	f _ Nothing       = Nothing
	f [] m            = m
	f (i:is) (Just z) = f is (goDown i z) 

replace :: Pattern a -> Zipper a -> Zipper a
replace p (_, cs) = (p, cs)

replaceLength :: Length -> Zipper a -> Maybe (Zipper a)
replaceLength l (p, cs@((Crumb i _ _):_)) = f $ goUp (p, cs) where
	f (Just (Branch bs, cs)) = Just (Branch $ ls ++ (l,snd r):rs, cs) where
		(ls,r:rs) = genericSplitAt i bs
	f _                      = Nothing
replaceLength _ _                         = Nothing

-- convert to rose tree
toRose :: Pattern a -> Tree (Length, Maybe a)
toRose = f 1 where
	f l (Leaf x) = Node (l, Just x) []
	f l (Branch pairs) = Node (l, Nothing) $ map (\(l,p) -> f l p) pairs

-- pretty print tree
instance (Show a) => Show (Pattern a) where
	show p = (draw p) . (fmap f) . toRose $ p where
		draw (Leaf _) = drawVerticalTree
		draw _        = drawVerticalForest . subForest 
		f (l, Just x) = show l ++ show x
		f (l, Nothing) = show l
		
-- sample tree		
sampleTree = Branch rs where
	rs = [(9, Branch rs2), (6, Branch rs3)] 
	rs2 = [(1, Leaf $ Beat R B), (1, Leaf $ Beat R T), (1, Branch rs5)]
	rs5 = [(1, Leaf $ Beat L O), (1, Leaf $ Beat L O)]
	rs3 = [(1, Leaf $ Beat R B),(1, Branch rs4)]
	rs4 = [(2, Leaf $ Beat R B),(1, Leaf $ Beat L B),(1, Leaf $ Beat L B)]

{- sample rose tree representation

                  1
                  |
         -------------------
        /                   \
        3                   2
        |                   |
  -----------         ---------
 /    |      \       /         \
1RB  1RT     1      1RB        1
             |                 |
            ----           ---------
           /    \         /    |    \
          1LO  1LO       2RB  1LB  1LB
-}

					