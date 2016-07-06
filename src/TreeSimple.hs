module TreeSimple where

import Tree

data TreeS = Node TreeS TreeS | Leaf Int 

instance Tree TreeS where
--	leafSum :: TreeS -> Int
	leafSum (Node l r) = leafSum l + leafSum r
	leafSum (Leaf x) = x


instance Show TreeS where 
	show (Leaf x) = show x 
	show (Node l r) = "(" ++ (show l) ++ " " ++ (show r)++ ")" 

leftDepth :: TreeS -> Int
leftDepth x = 
	let lD d (Leaf x) = d
	    lD d (Node l r) = lD (d+1) l 
	in lD 0 x
