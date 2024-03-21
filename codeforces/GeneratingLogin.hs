module Main(main) where

-- computeLogin :: String -> String -> String
-- computeLogin (x:xs) (y:ys) = go xs y [x]
--     where
--         go :: String -> Char -> String -> String
--         go [] l r = r ++ [l]
--         go (x:xs) l r = if x >= l then go [] l r else go xs l (r ++ [x])

computeLoginBetter :: String -> String -> String
computeLoginBetter (x:xs) (y:_) = x : takeWhile (< y) xs ++ [y]  


main :: IO()
main = do
    inp <- getLine
    let (fname:lastname:_) = words inp :: [String]


    putStrLn $ computeLoginBetter fname lastname