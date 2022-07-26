module Random where

import System.Random
import Data.List
import Data.Bifunctor (bimap)

randomList :: Int -> [Int]
randomList seed = randoms (mkStdGen seed) :: [Int]

randomNumericValue :: Int -> (Float, Float) -> Float
randomNumericValue seed range =
  let
    roll = uniformR range
    rolls = unfoldr (Just . roll)
    pureGen = mkStdGen seed
  in
    head (take 1 (rolls pureGen))

randomAttack :: Int -> (Int, Int) -> Int
randomAttack seed range = floor (randomNumericValue seed (Data.Bifunctor.bimap fromIntegral fromIntegral range))

randomProbability :: Int -> Float
randomProbability seed = randomNumericValue seed (0.0, 1.0)