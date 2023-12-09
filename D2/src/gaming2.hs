import Data.Bifunctor (second)

biggerThan :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
biggerThan (a,b,c) (x,y,z) = a >= x && b >= y && c >= z

main :: IO ()
main = do
    file <- readFile "./../data/input2.txt"
    let input = lines file
    print input
    let games = map (snd . splitFirst ':') input
    print games
    let gamesWithRounds = map (split ';') games
    print gamesWithRounds
    let numericalGames = map analyse gamesWithRounds
    print numericalGames
    let roundsMultiplied = map multiply numericalGames
    print roundsMultiplied
    let summed = sum roundsMultiplied
    print summed

multiply :: (Int, Int, Int) -> Int
multiply (a,b,c) = a*b*c

calc :: [(String, [String])] -> [(Int, (Int, Int, Int))] -- Interpret the input to the relevant numbers
calc [] = []
calc ((a,b):xs) = (numberOfGame a, analyse b) : calc xs

check :: (Int,Int,Int) -> Bool -- we check wether the given game is valid
check x = (13,14,12) `biggerThan` x

analyse :: [String] -> (Int, Int, Int)
analyse = foldr (highest . (transform . split ',')) (0, 0, 0) -- split a set of rounds into a list of round

transform :: [String] -> (Int, Int, Int) -- each string contains "x green" -> (x, 0, 0), "y blue" -> (0, y, 0), "z red" -> (0, 0, z)
transform [] = (0, 0, 0)
transform (x:xs) = case second of
    "green" -> (toNumber first, 0, 0) `highest` transform xs
    "blue" -> (0, toNumber first, 0) `highest` transform xs
    "red" -> (0, 0, toNumber first) `highest` transform xs
    _ -> transform xs
    where first = fst (splitFirst ' ' $ trim x)
          second = noSpace (snd (splitFirst ' ' $ trim x))

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (== ' ')

noSpace :: String -> String
noSpace = filter (/= ' ')

highest :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
highest (a,b,c) (x,y,z) = (max a x, max b y, max c z)

adding :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
adding (a,b,c) (x,y,z) = (a+x, b+y, c+z)

numberOfGame :: String -> Int
numberOfGame s = toNumber . snd $ splitFirst ' ' s

toNumber :: String -> Int
toNumber s = read s :: Int

organise :: [(String, String)] -> [(String, [String])]
organise = map (second (split ';'))
-- organise [] = []
-- organise [(a,b)] = [(a, split ';' b)]

splitFirst :: Char -> String -> (String, String)
splitFirst c s = let (l, r) = break (== c) s in (l, drop 1 r)

split :: Char -> String -> [String]
split delimiter = foldr f [[]]
    where f c l@(x:xs)
            | c == delimiter = []:l
            | otherwise = (c:x):xs