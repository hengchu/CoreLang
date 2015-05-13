module CoreLang.Utils
(
  Addr,
  Heap,
  hInitial,
  hAlloc,
  hUpdate,
  hFree,
  hLookup,
  hSize,
  hNull,
  hIsNull,
  mapAccuml
) where

import Data.Maybe (fromMaybe)

type Addr = Int
type Heap a = (Int, [Int], [(Int, a)])

hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (_, [], _) _ = error "Heap: ran out of usable address."
hAlloc (size, next:free, cts) n =
  ((size+1, free, (next, n):cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n =
  (size, free, (a, n):remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a =
  (size-1, a:free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (_, _, cts) a =
  let value = lookup a cts
  in fromMaybe (error $ "Can't find " ++ showaddr a ++ " in heap") value

showaddr :: Int -> String
showaddr a = "#" ++ show a

hSize :: Heap a -> Int
hSize (size, _, _) = size

hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull = (== hNull)

remove :: (Eq k, Show k) => [(k, a)] -> k -> [(k, a)]
remove [] a = error $ "Attempt to update or free nonexistent address #" ++ show a
remove ((a', n):cts) a | a == a' = cts
                       | otherwise = (a', n) : remove cts a

mapAccuml :: (m -> a -> (m, b)) -> m -> [a] -> (m, [b])
mapAccuml f acc as = mapAccuml' f acc as []

mapAccuml' :: (m -> a -> (m, b)) -> m -> [a] -> [b] -> (m, [b])
mapAccuml' f acc [] bs = (acc, reverse bs)
mapAccuml' f acc (a:as) bs =
  let (acc', b) = f acc a
  in mapAccuml' f acc' as (b:bs)
