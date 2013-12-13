module Heap where


import qualified Data.Map.Strict as M


type Heap a = (Int,         -- number of objects
               [Int],       -- list of unused addresses
               M.Map Int a) -- mapping from addresses to objects

type Addr = Int

hInitial :: Heap a
hInitial = (0, [1..], M.empty)

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc  (size, next : free, cts) n   = ((size + 1, free, M.insert next n cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, M.insert a n cts)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a   = (size-1, a:free, M.delete a cts)

hLookup :: Heap a -> Addr -> a
hLookup (size, free, cts) a = case M.lookup a cts of
                                       Nothing -> error $ "can't find node " ++ showaddr a ++ " in heap"
                                       Just r  -> r

hAddressses :: Heap a -> [Addr]
hAddressses (_, _, cts) = M.keys cts

hSize :: Heap a -> Int
hSize (size, _, _) = size

showaddr :: Addr -> String
showaddr a = "#" ++ show a

hNull :: Addr
hNull = 0
