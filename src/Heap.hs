module Heap where


import qualified Data.Map.Strict as M


type Heap a = (Int,         -- number of objects
               [Int],       -- list of unused addresses
               M.Map Int a) -- mapping from addresses to objects

type Addr = Int

default(Int)
hInitial                             = (0, [1..], M.empty)
hAlloc  (size, next : free, cts) n   = ((size + 1, free, M.insert next n cts), next)
hUpdate (size, free,        cts) a n = (size, free, M.insert a n cts)
hFree   (size, free,        cts) a   = (size-1, a:free, M.delete a cts)
hLookup (size, free,        cts) a   = case M.lookup a cts of
                                         Nothing -> error $ "can't find node " ++ showaddr a ++ " in heap"
                                         Just r  -> r
hAddressses (_, _, cts) = M.keys cts
hSize (size, _, _) = size
hNull = 0
hIsnull = (==) 0
showaddr a = "#" ++ show a
