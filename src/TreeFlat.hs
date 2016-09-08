{-# LANGUAGE MultiParamTypeClasses, BangPatterns, TemplateHaskell, TypeFamilies, FlexibleInstances #-}
module TreeFlat where

import TreeSimple
import qualified Data.Vector.Unboxed as U
--import qualified Data.Vector.Unboxed.Mutable as M
--import Data.Vector.Unboxed.Deriving
import Data.Word
import Data.Bits
import Tree



type FlatTree = U.Vector Word32

forceBang :: FlatTree -> IO  FlatTree
forceBang !x = return ( id x)


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



instance Tree FlatTree where 
--	leafSum :: FlatTree -> Int
	leafSum tree = helper 0 0 where
			helper :: Int -> Int -> Int 
			helper i s | (tree U.! i) .&. 1 == 0 	= s + (fromIntegral $ (tree U.! i) `shiftR` 1)
				| otherwise   			= (helper (fromIntegral ((tree U.! i) `shiftR` 1))(helper  (i+1) s ) )



getFrozenSum :: FlatTree -> Int
getFrozenSum ft = unpack 0 0 where
	unpack i acc = decode i acc (U.unsafeIndex ft i)
	decode i acc v	| v .&. 1 == 0 = acc + (fromIntegral $ shiftR v 1)
			| otherwise    = let rtix = fromIntegral (shiftR v 1)
					   in unpack rtix $ unpack (i+1) acc



leafSumFaster :: FlatTree ->   Int
leafSumFaster ft = unpack 0 0
		where
		unpack :: Int -> Int -> Int
		unpack i acc =  decode i acc (U.unsafeIndex ft i)
		decode :: Int -> Int -> Word32 -> Int
		decode i acc v 	| v .&. 1 == 0	=  acc + (fromIntegral $ v `shiftR` 1)
				| otherwise	= do 
						let rtix = fromIntegral (v `shiftR` 1)
						unpack rtix (unpack (i+1) acc )
