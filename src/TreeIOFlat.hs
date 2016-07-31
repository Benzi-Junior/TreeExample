{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies, FlexibleInstances #-}
module TreeIOFlat where

import TreeSimple
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word
import Data.Bits




type TreeIOFlat = M.IOVector Word32

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



flatten :: TreeS -> IO TreeIOFlat
flatten t = do let sz = sizeof t
               arr <- M.new sz
               buildtree t 0 arr
               return arr
            where
               buildtree :: TreeS -> Int -> TreeIOFlat -> IO Int
               buildtree (Leaf n)     i arr = do M.write arr i (fromIntegral n `shiftL` 1)
                                                 return $ i+1
               buildtree (Node lt rt) i arr = do nextFree <- buildtree lt (i+1) arr
                                                 M.write arr i (fromIntegral (nextFree `shiftL` 1) .|. 1)
                                                 buildtree rt nextFree arr
               sizeof (Leaf _) = 1
               sizeof (Node lt rt) = 1 + sizeof lt + sizeof rt


unflatten :: TreeIOFlat -> IO TreeS
unflatten ft = unpack 0
               where
                   unpack i = M.read ft i >>= decode i
                   decode i v | v .&. 1 == 0 = return $ Leaf (fromIntegral $ v `shiftR` 1)
                              | otherwise    = do lt <- unpack (i+1)
                                                  let rtix = fromIntegral (v `shiftR` 1)
                                                  rt <- unpack rtix
                                                  return $ Node lt rt

-- dumps the tree onto STDO one line for each  node split into the following  4 columns
-- index
-- value (as stored)
-- pointer (bit)
-- value (decoded)
dumptree :: TreeIOFlat -> IO ()
dumptree ft = go (M.length ft) 0
              where
                 go len i | i == len  = return ()
                          | otherwise = do v <- M.read ft i
                                           let flag = v .&. 1
                                           let val  = v `shiftR` 1
                                           putStrLn $ show i ++ "\t" ++ show v ++ "\t" ++ show flag ++ " " ++ show val 
                                           go len (i+1)






getLeafSum :: TreeIOFlat -> IO Int
getLeafSum ft = unpack 0
               where
                   unpack i = M.read ft i >>= decode i
                   decode i v | v .&. 1 == 0 = return $  (fromIntegral $ v `shiftR` 1)
                              | otherwise    = do lt <- unpack (i+1)
                                                  let rtix = fromIntegral (v `shiftR` 1)
                                                  rt <- unpack rtix
                                                  return $  lt + rt


-- Although the tree structure is not considered (beyond the checkbit) this algorithm goes through
-- the tree in depth first order (and is effectively tail recursive)

getLeafSumFaster :: TreeIOFlat -> IO Int
getLeafSumFaster ft = accum 0 0 ((M.length ft)-1)
               where
                   accum s i e = do
                                   v <- M.read ft i
                                   let x = if (v .&. 1 ==0 ) then (fromIntegral (shiftR v 1)) else (0)
                                   if (i==e) then (return (s+x)) else (accum (x+s) (i+1) e)


{-
getLeafSumAcc :: TreeIOFlat -> IO Int
getLeafSumAcc ft
    = unpack 0 0
      where
          unpack i acc = M.unsafeRead ft i >>= decode i acc
          decode !i !acc !v | v .&. 1 == 0 = return $ acc + (fromIntegral $ v `shiftR` 1)
                            | otherwise    = do let rtix = fromIntegral (v `shiftR` 1)
                                                unpack (i+1) acc >>= unpack rtix
-}
