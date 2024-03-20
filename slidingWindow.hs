module Main (main) where

-- import qualified Data.Sequence as Seq

-- maxSlidingWindow :: [Int] -> Int -> [Int]
-- maxSlidingWindow arr k = ans
--   where
--     ans = go Seq.empty (take k arr) [maximum (take k arr)]
--     go deq (x:xs) (y:ys) = go (updateDeq deq x) xs (newMax deq xs y : y : ys)
--     go deq [] (y:ys) = reverse (y : ys)
--     updateDeq deq x = Seq.dropWhileR (\i -> arr Seq.! i <= x) deq Seq.|> length arr
--     newMax deq xs y = maximum (y : [arr Seq.! i | i <- Seq.takeWhileL (\i -> i < length xs + k) deq])

main :: IO ()
main = do
  let arr = [2, 3, 7, 9, 5, 1, 6, 4, 3]
      k = 3
      result = 1
  print result












