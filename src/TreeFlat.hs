{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies, FlexibleInstances #-}
module TreeFlat where

import TreeSimple
import qualified Data.Vector.Unboxed as U
--import qualified Data.Vector.Unboxed.Mutable as M
--import Data.Vector.Unboxed.Deriving
import Data.Word
import Data.Bits




type FlatTree = U.Vector Word32


treeToList :: TreeS -> [Word32]
treeToList x = helper 1 x [] where
			helper :: Int -> TreeS -> [TreeS] -> [Word32]
			helper pos (Leaf n) [] = (fromIntegral (2*n)) : [] 
			helper pos (Leaf n) (y:ys) = (fromIntegral (2*n)) : helper (pos+1) y ys
			helper pos (Node l r) xs = (fromIntegral (2* ((sizeof l)+pos)+1 )) : helper (pos+1) l (r:xs) where 
				sizeof (Leaf _) = 1
				sizeof (Node lt rt) = 1 + sizeof lt + sizeof rt


plainFlatten :: TreeS -> FlatTree
plainFlatten tree = U.fromList   (treeToList tree)


flatSum :: FlatTree -> Int
flatSum tree = h 0 where
			h i | (tree U.! i) .&. 1 == 0 = (fromIntegral $ (tree U.! i) `shiftR` 1)
			    | otherwise   		= (h (i+1)) + (h (fromIntegral ((tree U.! i) `shiftR` 1)))


