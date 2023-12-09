main :: IO ()
main = do
    -- Read input
    file <- readFile "./../data/input.txt"
    let input = lines file

    let time = map read $ tail . words $ head input :: [Int]
    let distance = map read $ tail . words $ head $ tail input :: [Int]
    let race = zip time distance

    print $ product $ map (\(t,d) -> length [x | h <- [1 .. t], let x = (t - h) * h, x > d]) race