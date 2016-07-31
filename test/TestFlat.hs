module TestFlat where

import Lib
import Tree
import TreeSimple
import TreeFlat
import Parser
import System.Environment
import System.Clock
import Formatting
import Formatting.Clock
import Control.DeepSeq

{-
instance NFData TreeS where
  rnf (Leaf _) = ()
  rnf (Node lt rt) = rnf lt `seq` rnf rt `seq` ()
-}
runtest :: String -> IO ()
runtest fileName = do
	putStrLn "\nRunning tests and benchmarks for TreeFlat"
	strm <- readFile  fileName
	start <- getTime Monotonic

	let tree =   force  ((plainFlatten . doParse) $!  strm)
	construct <- getTime Monotonic

	putStr "Tree constructed in: "
	fprint  (timeSpecs % string) start construct "\n"
	let sum = leafSumFaster tree
	putStrLn $ "Sum computed: " ++ (show sum)
	first <- getTime Monotonic
	putStr "LeafSum computed in: "
	fprint  (timeSpecs % string) construct first "\n"
	let sum2 = leafSumFaster tree
	putStrLn $ "Sum computed: " ++ (show sum2)
	end <- getTime Monotonic
	putStr "repeat computation run in: " 
	fprint  (timeSpecs % string) first end "\n"

