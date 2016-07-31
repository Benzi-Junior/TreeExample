import TestIOFlat
import TestFlat
--import TestOpt
import TestSimple
--import TreeO

main :: IO ()
main = do
	putStrLn "Running test for Simple"
	TestSimple.runtest "test/TreeSample"
--	putStrLn "Running test for Opt"
--	TestOpt.runtest "test/TreeSample"
	putStrLn "Running test for Flat"
	TestFlat.runtest "test/TreeSample"
	putStrLn "Running test for IOFlat"
	TestIOFlat.runtest "test/TreeSample"
