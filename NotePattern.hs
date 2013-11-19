module NotePattern (
	Pattern(..), fold, tmap, normalize, replace, serialize, build, 
	goUp, goDown, toTop, focus, sampleTree ) where

import Rhythm

import Data.List (genericSplitAt, genericLength)
import Data.Tree
import Data.Tree.Pretty -- install pretty-tree and boxes
import Data.Maybe (fromJust)

-- leaf-centric implementation with absolute note length
data Pattern a = Leaf Length a | Branch [Pattern a]

-- fold all leafs of a tree
fold :: (b -> Length -> a -> b) -> b -> Pattern a -> b
fold f acc (Leaf l x)  = f acc l x
fold f acc (Branch ps) = foldr (flip $ fold f) acc ps

-- map all leafs of a tree
tmap :: (Length -> a -> (Length, b)) -> Pattern a -> Pattern b
tmap f (Leaf l x)  = uncurry Leaf $ f l x 
tmap f (Branch ps) = Branch $ map (tmap f) ps

{- normalize tree
- divide all leaf lengths by common divisor
- prune empty branches (except root node)
- prune leafs with length 0 -}
normalize :: Pattern a -> Pattern a
normalize p = tmap (\l x -> (div l g, x)) $ f p where
	-- gcd of all tree node length
	g = max 1 $ fold (\acc l _ -> gcd acc l) 0 p
	-- point free version: g = max 1 $ fold ((const .) . gcd) 0 p

	-- filter empty branches and length 0 leafs
	f (Branch ps) = Branch $ filter pred $ map f ps where
		pred (Branch []) = False
		pred (Leaf 0 _)  = False
		pred _           = True
	f (Leaf 0 _) = Branch [] -- replace by empty branch
	f p = p -- copy

-- bread crumb, index and length of taken subtree, discarded subtrees
data Crumb a = Crumb Index [Pattern a]

instance Show (Crumb a) where
	show (Crumb i _) = show i
	
type Zipper a = (Pattern a, [Crumb a])

-- descend at subtree referenced by index	
goDown :: Index -> Zipper a -> Maybe (Zipper a)
goDown i (Branch bs, cs) 
	| i >= genericLength bs = Nothing
	| otherwise             = Just (r, (Crumb i (ls ++ rs)):cs) where
		(ls,r:rs) = genericSplitAt i bs
goDown _ _ = Nothing

-- one level up
goUp :: Zipper a -> Maybe (Zipper a)
goUp (p, (Crumb i bs):cs) = Just ((Branch $ ls ++ p:rs), cs) where
	(ls,rs) = genericSplitAt i bs
goUp _                    = Nothing
	
-- go back to top	
toTop :: Zipper a -> Zipper a
toTop z@(_, []) = z
toTop z         = toTop $ fromJust $ goUp z

-- follow trace
focus :: [Index] -> Pattern a -> Maybe (Zipper a)
focus is p = f is $ Just (p, []) where
	f _ Nothing       = Nothing
	f [] m            = m
	f (i:is) (Just z) = f is (goDown i z) 
	
replace :: Pattern a -> Zipper a -> Zipper a
replace p (_, cs) = (p, cs)	

-- serialize tree
serialize :: Pattern a -> [(Length, a, [Index])]
serialize = f [] [] where
	f acc is (Branch ps) = foldr g acc $ zip [0..] ps where
		g (i,p) acc = f acc (i:is) p
	f acc is (Leaf l x) = (l, x, reverse is):acc
	
-- build tree from list
build :: [(Length, a, [Index])] -> Pattern a
build = foldl (\acc (l,x,is) -> f acc is $ Leaf l x) $ Branch [] where
	-- replace or insert subtree
	f (Branch ps) (i:is) p = Branch $ lhs ++ rhs' where
		(lhs,rhs) = genericSplitAt i ps
		rhs'
			| null rhs  = [f (Branch []) is p]
			| otherwise = (f (head rhs) is p):(tail rhs)
	f _ _ p = p

-- convert to rose tree
toRose :: Pattern a -> Tree (Maybe (Length, a))
toRose (Branch ps) = Node Nothing $ map toRose ps
toRose (Leaf l x)  = Node (Just (l,x)) []

-- pretty print
instance (Show a) => Show (Pattern a) where
	show p = (draw p) . (fmap f) . toRose $ p where
		draw (Leaf _ _) = drawVerticalTree
		draw _          = drawVerticalForest . subForest 
		f (Just (l,x)) = show l ++ show x
		f Nothing      = ""

sampleTree = Branch ps where
	ps = [Branch ps2, Branch ps3]
	ps2 = [Leaf 12 $ Beat R B, Leaf 12 $ Beat R T, Branch ps5]
	ps5 = [Leaf 6 $ Beat L O, Leaf 6 $ Beat L O]
	ps3 = [Leaf 12 $ Beat R B, Branch ps4]
	ps4 = [Leaf 6 $ Beat R B, Leaf 3 $ Beat L B, Leaf 3 $ Beat L B]
		
{-

                    |
          ---------------------
         /                     \

         |                     |
  -------------         ----------
 /     |       \       /          \
12RB  12RT            12RB
               |                  |
              ----            ---------
             /    \          /    |    \
            6LO  6LO        6RB  3LB  3LB
-}				