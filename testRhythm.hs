{-# LANGUAGE TemplateHaskell #-}
-- needed for running all tests

import Pattern -- my module to test
import qualified LengthPattern as L
import qualified NotePattern as N
import Rhythm

import Data.List
import Control.Monad
import Test.QuickCheck -- for writing tests
import Test.QuickCheck.All -- for running all tests starting with prop_

runTests :: IO Bool
--runTests = $verboseCheckAll -- for verbose test
runTests = $quickCheckAll  -- for mute test

main :: IO ()
main = runTests >>= \passed -> if passed then putStrLn "All tests passed."
                                         else putStrLn "Some tests failed."

-- draw random beat													  
instance Arbitrary Beat where
	arbitrary = do
		h <- oneof [return R, return L]
		n <- oneof [return B, return O, return T]
		return $ Beat h n
	
-- draw random length pattern 	
instance (Arbitrary a) => Arbitrary (L.Pattern a) where
	arbitrary = f 1 where
		chooseLeaf = liftM L.Leaf arbitrary
		f n = frequency [(n, chooseLeaf), (1, chooseRight)] where
			maxBranch = 5 -- max number of branches
			maxLength = 10 -- max length of branch
			chooseRight = do
				c  <- choose (0, maxBranch) -- number of branches
				ls <- vectorOf c $ choose (0, maxLength)
				bs <- vectorOf c $ f $ 2*n
				return . L.Branch $ zip ls bs
				
instance (Arbitrary a) => Arbitrary (N.Pattern a) where
	arbitrary = f 1 where
		--maxLength = 10 -- max length of branch
		chooseLeaf = do
			l <- choose (0, maxLength)
			x <- arbitrary
			return $ N.Leaf l x where
				maxLength = 10 -- max length of branch
			
		f n = frequency [(n, chooseLeaf), (1, chooseBranch)] where
			maxBranch = 5 -- max number of branches
			chooseBranch = do
				c  <- choose (0, maxBranch) -- number of branches
				bs <- vectorOf c $ f $ 2*n
				return . N.Branch $ bs
	shrink (N.Branch bs) = [ b | b <- bs ]

{- test generators with: 
sample' arbitrary :: IO [L.Pattern Beat]
or 
ts <- sample' arbitrary :: IO [N.Pattern Beat] 
-}

{- check normalize length pattern
- structural equivalence of org and normalized tree 
- ratios of branch lengths are constant
- branch lengths are relatively prime -}
prop_L_normalize :: L.Pattern Beat -> Bool
prop_L_normalize t = f t $ L.normalize t where
	f (L.Leaf x) (L.Leaf x')       = x == x'
	f (L.Branch bs) (L.Branch bs') = all ($ ls) props  where
		-- skip orig branches with length 0
		fbs = filter ((/= 0) . fst) bs
		
		-- zip length ratios and recursive evaluation of branches
		ls :: ([(Length, Length)], [Bool])
		ls = unzip $ map (uncurry g) $ zip fbs bs' where
			g (l, b) (l', b') = ((l, l'), f b b')

		props = [and . snd, ratio . fst, prime . fst, len . snd]
		
		len ls = (length ls) == (length fbs)
		
		ratio [] = True
		ratio (l:ls) = all ((u l) ==) $ map u ls where
			u = uncurry div
		
		prime [] = True
		prime ls = (foldr1 gcd $ map snd ls) == 1
	f _ _ = False
	
{- check normalize note pattern
- structural equivalence of org and normalized tree
- ratios of note lengths are constant
- note lengths are relatively prime -}
prop_N_normalize :: N.Pattern Beat -> Bool
prop_N_normalize t = (lgcd t' == 1) && (f t t') where
	t' = N.normalize t
	lgcd = (max 1) . (N.fold (\acc l _ -> gcd acc l) 0)
	g = lgcd t
	f (N.Leaf 0 _) (N.Branch [])   = True
	f (N.Leaf l x) (N.Leaf l' x')  = (div l l' == g) && (x == x')
	f (N.Branch bs) (N.Branch bs') = ((length fbs) == (length bs')) && cond where
		cond = all (uncurry f) $ zip fbs bs' 
		fbs = filter pred bs where
			pred (N.Leaf 0 _)  = False
			pred (N.Branch bs) = any pred bs
			pred _             = True
	f _ _ = False
			