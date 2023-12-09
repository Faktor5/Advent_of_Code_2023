main :: IO ()
main = do
    -- Read input
    file <- readFile "./../data/input.txt"
    let input = lines file

    let time = read . filter (/= ' ') . unwords . tail . words $ head input :: Int
    let distance = read . filter (/= ' ') . unwords . tail . words $ head $ tail input :: Int
    
    print time
    print distance
    print $ length [x | h <- [1 .. time], let x = (time - h) * h, x > distance]