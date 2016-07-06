module Main where

import Lib
import Tree
import TreeSimple
--import TreeSample
import TreeOpt
--import Data.Array.Unboxed
tree = (Leaf 2)
main :: IO ()
main = putStrLn (show (leafSum tree))



