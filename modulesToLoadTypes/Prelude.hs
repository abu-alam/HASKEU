aList		 :: [a] 
aVar		 :: a
listCons	 :: a -> [a] -> [a]
map              :: (b -> a) -> [b] -> [a]
foldr            :: (a -> b -> b) -> b -> [a] -> b
foldl            :: (a -> b -> a) -> a -> [b] -> a
reverse          :: [a] -> [a]
(+)              :: a -> a -> a
(-)              :: a -> a -> a
(*)              :: a -> a -> a
(>)              :: a -> a -> Bool
cond             :: Bool -> a -> a -> a
mapTT            :: (a -> b) -> [a] -> [b]
mapTD            :: (a -> b) -> (a -> b -> c)
max		 :: a -> a -> a
rev		 :: [a] -> [a] -> [a]

