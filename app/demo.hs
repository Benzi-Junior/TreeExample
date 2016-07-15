
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies, FlexibleInstances #-}

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Vector.Unboxed.Deriving
import Data.Word
import Data.Bits


-- First, the type declaration for the tree.

data RecTree = Leaf Int | Node RecTree RecTree deriving Show


-- Next, we define the type for an efficiently stored Tree.
-- I'm using IOVector rather than plain Vector so that I can update
-- in-place.  It would be possible to do this using a plain
-- unboxed Vector by first converting the tree to a list, then using
-- U.fromList, but unclear how well this would fuse ... perhaps
-- an exercise for you?

type FlatTree = M.IOVector Word32

-- We can't just convert between an isolated node in the recursive
-- tree and an entry in the compact array as converting a node
-- requires constructing multiple entries in the array, and
-- converting from a given array entry requires looking at
-- multiple entries in the array to get the left/right subtrees.

-- I am using the following encoding for nodes in the recursive tree:
-- Leaf n      <--->  (n << 1) & ~1
-- Node lt rt  <--->  (rt << 1) | 1
-- This is risky and would not work well if we are storing negative
-- values in nodes!

flatten :: RecTree -> IO FlatTree
flatten t = do let sz = sizeof t
               arr <- M.new sz
               buildtree t 0 arr
               return arr
            where
               buildtree :: RecTree -> Int -> FlatTree -> IO Int
               buildtree (Leaf n)     i arr = do M.write arr i (fromIntegral n `shiftL` 1)
                                                 return $ i+1
               buildtree (Node lt rt) i arr = do nextFree <- buildtree lt (i+1) arr
                                                 M.write arr i (fromIntegral (nextFree `shiftL` 1) .|. 1)
                                                 buildtree rt nextFree arr
               sizeof (Leaf _) = 1
               sizeof (Node lt rt) = 1 + sizeof lt + sizeof rt


unflatten :: FlatTree -> IO RecTree
unflatten ft = unpack 0
               where
                   unpack i = M.read ft i >>= decode i
                   decode i v | v .&. 1 == 0 = return $ Leaf (fromIntegral $ v `shiftR` 1)
                              | otherwise    = do lt <- unpack (i+1)
                                                  let rtix = fromIntegral (v `shiftR` 1)
                                                  rt <- unpack rtix
                                                  return $ Node lt rt


dumptree :: FlatTree -> IO ()
dumptree ft = go (M.length ft) 0
              where
                 go len i | i == len  = return ()
                          | otherwise = do v <- M.read ft i
                                           let flag = v .&. 1
                                           let val  = v `shiftR` 1
                                           putStrLn $ show i ++ "\t" ++ show v ++ "\t" ++ show flag ++ " " ++ show val 
                                           go len (i+1)
