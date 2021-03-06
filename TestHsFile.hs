foldrT f z []     = z 
foldrT f z (x:xs) = f x (foldrT f z xs) 
 
foldlT f z []     = z                  
foldlT f z (x:xs) = foldlT f (f z x) xs

mapT _ []     = []
mapT f (x:xs) = (:) (f x) (mapT f xs)


reverseT l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs ((:) x a)

module Prelude where

foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 
 
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs

map _ []     = []
map f (x:xs) = (:) (f x) (map f xs)

reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs ((:) x a)

reva l = rev l []
  where rev [] a = a
        rev (x : xs) a = rev xs ((:) x a)

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys
