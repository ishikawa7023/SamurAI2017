module Initialization where

import System.IO
import Information as Info

-- initial input example
-- 2000
-- 100
-- 15 100
-- 8
-- 0

getInitialInformation :: IO InitialInformation
getInitialInformation =
  fmap read getLine >>=
  \rt    -> fmap read getLine >>=
  \sl    -> fmap (map read . words) getLine >>=
  \[w,l] -> fmap read getLine >>=
  \vl    -> return (II rt sl w l vl)

initialOutput :: IO ()
initialOutput = putStrLn "0" >>= \_ -> hFlush stdout
