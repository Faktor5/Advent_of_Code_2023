import Data.List (intersect)

main :: IO ()
main = do
    file <- readFile "./../data/input 1.txt"
    print $ map organise $ lines file
    print $ map ((\e -> (frst e, scnd e `intersect` thrd e)) . organise) $ lines file
    print $ map ((\e -> length $ scnd e `intersect` thrd e) . organise) $ lines file
    print $ map (calc . (\e -> length $ scnd e `intersect` thrd e) . organise) (lines file)
    print $ sum $ map (calc . (\e -> length $ scnd e `intersect` thrd e) . organise) (lines file)

calc :: Int -> Int
calc 0 = 0
calc 1 = 1
calc n = 1 * 2^(n-1)

organise :: String -> (String, [Int], [Int])
organise input = (name,map toNumber $ words number,map toNumber $ words prize)
    where
        name = takeWhile (/= ':') input
        number = takeWhile (/= '|') $ drop (length name + 1) input
        prize = drop (length name + length number + 2) input

toNumber :: String -> Int
toNumber s = read s :: Int

frst :: (a, b, c) -> a
frst (x, _, _) = x

scnd :: (a, b, c) -> b
scnd (_, x, _) = x

thrd :: (a, b, c) -> c
thrd (_, _, x) = x