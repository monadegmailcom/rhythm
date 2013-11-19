module Pattern (
	convertNL, convertLN ) where

import Rhythm	
import qualified LengthPattern as L
import qualified NotePattern as N


-- convert from note to normalized length pattern
convertNL :: N.Pattern a -> L.Pattern a
convertNL = L.normalize . snd . f where
	f (N.Leaf l x) = (l,L.Leaf x)
	f (N.Branch ps) = (foldr ((+).fst) 0 ls, L.Branch ls) where
		ls = map f ps

-- convert from length to normalized note pattern		
-- ugly, but useful
convertLN :: L.Pattern a -> N.Pattern a
convertLN = N.normalize . snd . f where 
	f (L.Leaf x)    = (1, N.Leaf 1 x)
	f (L.Branch ps) = (foldr ((+) . fst) 0 ps', N.Branch $ map snd ps') where
		pairs = map (f . snd) ps
		pr = foldr ((*) . fst) 1 pairs
		g (l',_) (l,p) = (q*l, N.tmap (\u x -> (q*u, x)) p) where
			q = l' * (div pr l)
		ps' = zipWith g ps pairs
	