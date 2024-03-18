module Main(main) where

import Data.List (tails)
import Control.Monad ( replicateM )

f :: [a] -> [a]
f (a:b:c) = b:f c
f _ = []


test :: Int -> [Int] -> Bool
test n xs =
  let root = exp (sum (map log (fromIntegral <$> xs)) / fromIntegral n)
      roundedRoot = round root
  in abs (root - fromIntegral roundedRoot) <= 0.00001 && all (\x -> x == 1 || gcd x roundedRoot /= 1) xs


task :: [String] -> [Bool]
task [] = []
task arr = go arr []
    where
        go [] res = res
        go (x:xs) res = go xs (test (length x) (read x):res) 


printResult :: Bool -> IO ()
printResult b = if b then putStrLn "Yes" else putStrLn "No"


main :: IO ()

main = do
    ni <- getLine
    let n = read ni :: Int
    inp <- replicateM (n*2) getLine
    mapM_ printResult $ task (f inp)