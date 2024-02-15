module Main (main) where

packageList :: Int -> [Int]
packageList 0 = []
packageList n = n : packageList (n - 1)

main :: IO ()
main = do
     input <- getLine
     let inputArr = map read(words input) :: [Int]
     let (size:nPackets:_) = inputArr
     let test = packageList(nPackets)

     putStrLn $ show size ++ " " ++ show nPackets ++ "\n array: " ++ show test
     