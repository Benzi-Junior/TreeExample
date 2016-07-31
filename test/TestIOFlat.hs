module TestIOFlat where

import Lib
import Tree
import TreeSimple
import TreeIOFlat
-- import TreeOpt
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
--	fileName <- getArgs
	strm <- readFile fileName
	-- let tree = doParse strm
	let tree = doParse strm -- force $ doParse strm
	iot <- flatten tree
	start <- getTime Monotonic
        sum <- getLeafSum iot
	putStrLn $ "Sum Computed: " ++ (show sum) -- (leafSum tree)
	end <- getTime Monotonic
	fprint (timeSpecs % string) start end "\n"
	putStrLn "Running test for faster method (ignoring tree structure)"
	startf <- getTime Monotonic
        sumf <- getLeafSumFaster iot
	putStrLn $ "Sum Computed: " ++ (show sumf) -- (leafSum tree)
	endf <- getTime Monotonic
	fprint (timeSpecs % string) startf endf "\n"




