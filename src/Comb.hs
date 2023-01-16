module Comb where

comb :: Int -> [a] -> [[a]]
comb 0 _       = [[]]
comb _ []      = []
comb k (x:xs)  = [ x:ys | ys <- comb (k-1) xs]
              ++ [ zs   | zs <- comb k xs ]
