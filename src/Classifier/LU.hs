module Classifier.LU
    ( lu,luSolve
    ) where

import Data.List

lu :: [[Double]] -> ([[Double]], [[Double]])
lu a = reclu a ([], map (\x->[]) [1..(length a)])

reclu :: [[Double]] -> ([[Double]],[[Double]]) -> ([[Double]], [[Double]])
reclu [] (l, ut) = (l, transpose ut)
reclu (ai:arst) (l, ut) = reclu arst (l++[nl], zipWith (\x -> \y -> x++[y]) ut nu)
                            where (nl, nu) = sublu (length l) ai ut ([],[])


sublu :: Int -> [Double] ->[[Double]]-> ([Double], [Double]) -> ([Double], [Double])
sublu i [] _ (l, u) = (l,u)
sublu i (aij:airst) (utj:uttl) (li, uj) = sublu i airst uttl (li++[nl], uj++[nu])
                                          where j = length li
                                                sm = aij - (foldl (+) 0 (zipWith (*) li utj))
                                                (nl, nu) | i==j = (1,sm)
                                                         | i<j = (0,sm)
                                                         | i>j = (sm/(utj!!j),0)

forwardSub ::[[Double]] -> [Double] -> [Double]-> [Double]
forwardSub [] _ c = c
forwardSub (lh:lt) (bh:bt) c = forwardSub lt bt (c++[h])
                                where h = bh  - (foldl (+) 0 (zipWith (*) lh c))

backwardSub ::[[Double]] -> [Double]-> [Double]
backwardSub [] _ = []
backwardSub (lh:lt) (bh:bt) =  (h / (lh!!k) ):c
                                where  c = backwardSub lt bt
                                       k = length lh - length c - 1
                                       h = bh - (foldl (+) 0 (zipWith (*) (reverse lh) (reverse c)))

luSolve :: [[Double]] -> [Double] -> [Double]
luSolve a b = (backwardSub u (forwardSub l b []))
            where (l,u) = lu a
