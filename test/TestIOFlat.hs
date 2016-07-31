module TestIOFlat where

import Lib
import Tree
import TreeSimple
import TreeIOFlat
import Parser
import System.Environment
import System.Clock
import Formatting
import Formatting.Clock
import Control.DeepSeq

runtest :: String -> IO ()
runtest fileName = do
	putStrLn "\nRunning tests and benchmarks for TreeIOFlat"
	strm <- readFile fileName
	start <- getTime Monotonic
	let tree = doParse strm -- force $ doParse strm
	iot <- flatten tree
	construct <- getTime Monotonic
	putStr "Tree constructed in: " 
	fprint (timeSpecs % string) start construct "\n"
        sum <- getLeafSum iot
	putStrLn $ "Sum Computed: " ++ (show sum) -- (leafSum tree)
	first <- getTime Monotonic
	fprint (timeSpecs % string) construct first "\n"
        sum2 <- getLeafSum iot
	putStrLn $ "Sum Computed: " ++ (show sum) -- (leafSum tree)
	end <- getTime Monotonic
	fprint (timeSpecs % string) first end "\n"
	

{-
	putStrLn "Running test for faster method (ignoring tree structure)"
	startf <- getTime Monotonic
        sumf <- getLeafSumFaster iot
	putStrLn $ "Sum Computed: " ++ (show sumf) -- (leafSum tree)
	endf <- getTime Monotonic
	fprint (timeSpecs % string) startf endf "\n"

-}


