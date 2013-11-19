-- rhythm patterns as trees
module Rhythm (
	Length, Index, Hand(..), Note(..), Beat(..), (-:) ) where

import Data.Word -- use unsigned Int

type Length = Word -- time length of a pattern
type Index = Word -- 0 based index for subtree selection

-- rhythm as tree
data Hand = R|L deriving (Show, Eq) -- right/left hand
data Note = B|O|T deriving (Show, Eq) -- bass, open, tip
data Beat = Beat { hand :: Hand, note :: Note }  deriving Eq

instance Show Beat where
	show (Beat h n) = show h ++ show n

(-:) :: a -> (a -> b) -> b
(-:) = flip ($)
