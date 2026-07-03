module Util where

import Control.Monad (liftM2)

newline :: IO ()
newline = putStrLn ""

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

consM :: Monad m => m a -> m [a] -> m [a]
consM = liftM2 (:)
