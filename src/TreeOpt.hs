module TreeOpt where

import TreeSimple
import Tree
--import Data.Array
import Data.Array.Unboxed
--import Data.Array.IArray

data TreeO = RootNode (Array Int TreeON) 
data TreeON = RightPointer TreeO | OLeaf Int


getLeftWing :: TreeS -> [TreeON]
getLeftWing (Leaf x)  = [OLeaf x]
getLeftWing (Node l r) = (RightPointer   (unboxTree r) ) : (getLeftWing l)

unboxTree :: TreeS -> TreeO
unboxTree (Leaf x) =RootNode ( array (0,0) [(0,OLeaf x)])
unboxTree  n  = RootNode (array (0,leftDepth n) (indexList (getLeftWing n))) 



boxTree :: TreeO -> TreeS 
boxTree (RootNode x) = boxNode (elems x) 


boxNode :: [TreeON] -> TreeS
boxNode ((RightPointer ar):xs) = Node (boxNode xs) (boxTree ar)
boxNode [OLeaf x] = Leaf x





indexList :: [a] -> [(Int,a)]
indexList x  =  accumelatedindex 0 x 
	where   accumelatedindex i [] = []
		accumelatedindex i (x:xs) = (i,x):(accumelatedindex (i+1) xs)
{-
instance IArray UArray TreeS where 
	{-# INLINE bounds #-}
	bounds (UArray l u _ _) = (l,u)
	{-# INLINE numElements #-}
	numElements (UArray _ _ n _) = n
	{-# INLINE unsafeArray #-}
	unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)
	{-# INLINE unsafeAt #-}
	unsafeAt (UArray _ _ _ arr#) (I# i#) = I# (indexIntArray# arr# i#)
	{-# INLINE unsafeReplace #-}
	unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
	{-# INLINE unsafeAccum #-}
	unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
	{-# INLINE unsafeAccumArray #-}
	unsafeAccumArray f initialValue lu ies = runST (unsafeAccumArrayUArray f initialValue lu ies)

-}
instance Tree TreeO where
	--leafSum :: Tree -> Int
	--leafSum LeftArray = 0
	leafSum (RootNode arr) = (sum (amap nodeValue arr)) where 
		nodeValue :: TreeON -> Int
		nodeValue (RightPointer right) = leafSum right
		nodeValue (OLeaf x) = x

firstlove :: Array Int Int
firstlove = array (0, 4) [(i, 0) | i <- [0..4]]
