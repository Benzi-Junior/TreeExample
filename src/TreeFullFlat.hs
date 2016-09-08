{-# LANGUAGE MultiParamTypeClasses, BangPatterns, TemplateHaskell, TypeFamilies, FlexibleInstances #-}
module TreeFullFlat where

import TreeSimple
import qualified Data.Vector.Unboxed as U
import Data.Word
import Data.Bits
import Tree



type FullTree = U.Vector Word32

forceBang :: FullTree -> IO  FullTree
forceBang !x = return ( id x)


treeToList :: TreeS -> [Word32]
treeToList x = helper 2 x [] where
			helper :: Int -> TreeS -> [TreeS] -> [Word32]
			helper pos (Leaf n) [] = (fromIntegral 0) : (fromIntegral (n)) : [] 
			helper pos (Leaf n) (y:ys) = (fromIntegral 0) : (fromIntegral (n)) : helper (pos+2) y ys
			helper pos (Node l r) xs = (fromIntegral 1) : (fromIntegral ( ((sizeof l)+pos) )) : helper (pos+2) l (r:xs) where 
				sizeof (Leaf _) = 2
				sizeof (Node lt rt) = 2 + sizeof lt + sizeof rt



plainFlatten :: TreeS -> FullTree
plainFlatten tree = U.fromList   (treeToList tree)



instance Tree FullTree where 
	leafSum tree = helper 0 0 where
			helper :: Int -> Int -> Int 
			helper i s 	| (tree U.! i)  == 0 = s + (fromIntegral $ (tree U.! (i+1)))
					| (tree U.! i)  == 1 = (helper (fromIntegral (tree U.! (i+1))) (helper  (i+2) s))

punt :: FullTree -> IO Int
punt tree = helper 0 0 where
		helper :: Int -> Int -> IO Int
		helper i s 	| (tree U.! i)  == 0 =do
							 putStrLn (show (i,s))
							 return (s + (fromIntegral  (tree U.! (i+1))))
				| (tree U.! i)  == 1 = do
							putStrLn (show (i,s)) 
							(helper  (i+2) s)>>=(helper (fromIntegral (tree U.! (i+1))) )

getFrozenSum :: FullTree -> Int
getFrozenSum ft = unpack 0 0 where
	unpack i acc = decode i acc (U.unsafeIndex ft i)
	decode i acc v	| v .&. 1 == 0 = acc + (fromIntegral $ shiftR v 1)
			| otherwise    = let rtix = fromIntegral (shiftR v 1)
					   in unpack rtix $ unpack (i+1) acc



leafSumFaster :: FullTree ->   Int
leafSumFaster ft = unpack 0 0
		where
		unpack :: Int -> Int -> Int
		unpack i acc =  decode i acc (U.unsafeIndex ft i)
		decode :: Int -> Int -> Word32 -> Int
		decode i acc v 	| v .&. 1 == 0	=  acc + (fromIntegral $ v `shiftR` 1)
				| otherwise	= do 
						let rtix = fromIntegral (v `shiftR` 1)
						unpack rtix (unpack (i+1) acc )
