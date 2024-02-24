module Main (main) where
import Control.Monad

processPackages :: Int -> [(Int, Int)] -> [Int]
processPackages size packets = go [] packets 0
  where
    go :: [(Int, Int)] -> [(Int, Int)] -> Int -> [Int]
    go _ [] _ = []
    go buffer ((arrival, duration):rest) currentTime =
      let
         (buffer', startTimes) = processBuffer buffer arrival currentTime
         startTime = if length buffer' < size then max currentTime arrival else -1
      in
         startTime : go buffer' rest (max currentTime arrival + duration)

    processBuffer :: [(Int, Int)] -> Int -> Int -> ([(Int, Int)], [Int])
    processBuffer buffer arrival currentTime =
      let
         (valid, rest) = span (\(start, _) -> start <= arrival) buffer
         buffer' = drop (length valid) rest ++ [(currentTime + duration, currentTime + duration) | (_, duration) <- valid]
      in
         (buffer', map fst valid)


main :: IO ()
main = do
     input <- getLine
     let (size:nPackets:_) = map read $ words input :: [Int]

     packets <- replicateM nPackets $ do
        line <- getLine
        let (arrivalTime:processingTime:_) = map read $ words line :: [Int]
        return (arrivalTime, processingTime)

     mapM_ print (processPackages size packets)

