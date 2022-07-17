module Random where

import System.Random
import Data.List

randomList :: Int -> [Int]
randomList seed = randoms (mkStdGen seed) :: [Int]

randomAttack :: Int -> (Int, Int) -> Int
randomAttack seed range =
  let
    roll = uniformR range
    rolls = unfoldr (Just . roll)
    pureGen = mkStdGen seed
  in
    head (take 1 (rolls pureGen))

randomProbability :: Int -> Float
randomProbability seed =
  let
    roll = uniformR (0.0, 1.0)
    rolls = unfoldr (Just . roll)
    pureGen = mkStdGen seed
  in
    head (take 1 (rolls pureGen))