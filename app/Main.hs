module Main where

import Lib
import Tree
import TreeSimple
--import TreeSample
import TreeOpt
import Parser
import System.Environment
--import Data.Array.Unboxed
--tree = (Leaf 2)
main :: IO ()
main = do
	fileName <- getArgs
	skrá <- readFile (head fileName)
	let tree = doParse skrá
	putStrLn (show tree)



