module Classifier.MultiClassFisher
    ( train
    ) where

import Classifier.LU
import Data.List



divide :: Integer -> ([[Double]], [Integer]) -> ([[Double]],[[Double]])
divide i ([],_) = ([],[])
divide i (smp:srst, lb:lrst)  | lb == i   = (smp:ci, others)
                              | otherwise = (ci, smp:others)
                              where (ci, others) = divide i (srst, lrst)

average :: [[Double]] -> [Double]
average smps = map (/realToFrac(length(smps))) (foldl1 (zipWith (+)) smps)

calcSw :: [Double] -> [[Double]] -> [[Double]]
calcSw m [] = map (\x -> (map (\y -> 0) [1..ln])) [1..ln]
              where ln = length m
calcSw m (ch:ct) =  zipWith (\l1 -> \l2 -> zipWith (+) l1 l2) sw oth
      where xm = zipWith (-) ch m
            sw = map (\x -> map (*x) xm) xm
            oth = calcSw m ct


train2c :: Integer -> ([[Double]], [Integer]) -> [Double] -> Double
train2c n (smps, lbs) = \v -> ((foldl1 (+) (zipWith (*) w v)) - w0)
  where (c0,c1) = divide 0 (smps, lbs)
        m0 = average c0
        m1 = average c1
        m = zipWith (-) m1 m0
        sw = zipWith (\l1 -> \l2 -> zipWith (+) l1 l2) (calcSw m0 c0) (calcSw m1 c1)
        w = luSolve sw m
        w0 = foldl (+) 0 (zipWith (*) w (map (/2.0) (zipWith (+) m1 m0)))

uni :: [Integer] -> [Integer] -> [Integer]
uni [] rs = rs
uni (lb:lbs) rs = if (elem lb rs) then (uni lbs rs) else (uni lbs (lb:rs))


train ::([[Double]], [Integer]) -> [Double] -> Integer
train (smps, lbs) v = ans
            where ulb = uni lbs []
                  rst = map (\f-> f v) (map (\c2 -> c2 (smps, lbs)) (map train2c ulb))
                  pair = foldr1 (\(v0,l0)-> \(v1,l1) -> if v0<v1 then (v1,l1) else (v0,l0)) (zipWith (\x -> \y ->(x,y)) rst ulb)
                  ans = (\(v,l) -> l) pair
