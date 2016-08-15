module TreeGen where

import TreeSimple

--data TreeS = Node TreeS TreeS | Leaf Int 




genFull -> Int -> Int -> TreeS
genfull 0 x = Leaf x
genfull d x = Node (genFull (d-1) x) (genFull (d-1) x)


