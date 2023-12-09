import Data.List (isPrefixOf)
import Data.Char (isDigit)

normalDigits = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]
reversDigits = [("eno",1), ("owt",2), ("eerht",3), ("ruof",4), ("evif",5), ("xis",6), ("neves",7), ("thgie",8), ("enin",9)]

main :: IO ()
main = do
    file <- readFile "../data/input4.txt"
    let input = lines file
    print input
    let firstDigit = map (first normalDigits) input
    let lastDigit = map (first reversDigits . reverse) input
    let result = zipWith (\a b -> a * 10 + b) firstDigit lastDigit
    print result
    let summing = sum result
    print summing

first :: [(String, Int)] -> String -> Int
first _ [] = 0
first dict (x:xs)
    | isDigit x = toDigits [x]
    | isDigitWords dict (x:xs) = toDigitWords dict (x:xs)
    | otherwise = first dict xs

isDigitWords :: [(String, Int)] -> String -> Bool
isDigitWords [] _ = False
isDigitWords ((a,b):xs) s
    | a `isPrefixOf` s = True
    | otherwise = isDigitWords xs s

toDigitWords :: [(String, Int)] -> String -> Int
toDigitWords [] _ = 0
toDigitWords ((a,b):xs) s
    | a `isPrefixOf` s = b
    | otherwise = toDigitWords xs s

toDigits :: String -> Int
toDigits x = read x :: Int