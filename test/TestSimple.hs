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
	putStrLn "\nRunning tests and benchmarks for TreeSimple"
	strm <- readFile  fileName

	
	start <- getTime Monotonic
	let tree = force $ doParse strm
	--forceEval tree
	construct  <- getTime Monotonic
	putStr "Simple tree constucted in: " 
	fprint (timeSpecs % string) start construct "\n"
        let sum = leafSum tree
	putStrLn $ "Sum computed: " ++ (show sum)
	first <- getTime Monotonic
	putStr "Leafsum computed in: "
	fprint (timeSpecs % string) construct first "\n"
        let sum2 = leafSum tree
	putStrLn $ "Sum computed: " ++ (show sum2)
	end <- getTime Monotonic
	putStr "repeat computation run in: "
	fprint (timeSpecs % string) first end "\n"



