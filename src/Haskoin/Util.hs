module Haskoin.Util
  ( average
  , safeDiv
  , timed
  ) where

import qualified Data.Time.Clock.POSIX as POSIX


average :: (Foldable f, Num a, Fractional a, Eq a) => f a -> a
average xs = sum xs `safeDiv` fromIntegral (length xs)

safeDiv :: (Eq a, Fractional a) => a -> a -> a
safeDiv n d = n / (if d == 0 then 1 else d)

timed :: IO a -> IO a
timed action = do
  before <- POSIX.getPOSIXTime
  result <- action
  after <- POSIX.getPOSIXTime
  print ("Time", after - before)
  return result
