import TestIOFlat
import TestFlat
import TestOpt
import TestSimple
--import TreeO

main :: IO ()
main = do
	let fileName= "app/TreeSample" 
	TestSimple.runtest fileName
	TestOpt.runtest fileName
	TestFlat.runtest fileName
	TestIOFlat.runtest fileName
