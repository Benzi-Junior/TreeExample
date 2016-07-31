module TestSimple where

import Lib
import Tree
import TreeSimple
import TreeOpt
import Parser
import System.Environment
import System.Clock
import Formatting
import Formatting.Clock
import Control.DeepSeq


instance NFData TreeS where
  rnf (Leaf _) = ()
  rnf (Node lt rt) = rnf lt `seq` rnf rt `seq` ()

runtest :: String -> IO ()
runtest fileName = do
--	fileName <- getArgs
	strm <- readFile  fileName
	let tree = doParse $! strm
	--let tree = force $ doParse strm
	start <- getTime Monotonic
        let sum = leafSum tree
	putStrLn $ show sum
	end <- getTime Monotonic
	fprint (timeSpecs % string) start end "\n"




