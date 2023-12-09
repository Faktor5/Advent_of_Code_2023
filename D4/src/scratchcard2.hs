import Data.List (intersect)

main :: IO ()
main = do
    file <- readFile "./../data/example.txt"
    print $ map organise $ lines file -- all Cards
    print $ map ((\e -> (frst e, scnd e `intersect` thrd e)) . organise) $ lines file -- Cards and their winning numbers
    print $ map ((\e -> (frst e, length $ scnd e `intersect` thrd e, 1)) . organise) $ lines file -- Cards and their points
    print $ map (\e -> (frst e, thrd e)) . recalculateAmount . map ((\e -> (frst e,calc . length $ scnd e `intersect` thrd e, 1)) . organise) $ lines file -- Cards and their copies
    print $ sum $ map thrd (recalculateAmount . map ((\e -> (frst e, length $ scnd e `intersect` thrd e, 1)) . organise) $ lines file) -- sum of all originals and copies

recalculateAmount :: [(String, Int, Int)] -> [(String, Int, Int)]
recalculateAmount [] = []
recalculateAmount (x:xs) = x : recalculateAmount (next ++ rest)
    where amount = scnd x
          value = thrd x
          next  = map (increase value) $ take amount xs
          rest = drop amount xs

increase :: Int -> (String, Int, Int) -> (String, Int, Int)
increase n (name, points, amount) = (name, points, amount + n)

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