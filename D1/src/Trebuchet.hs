import Data.Char (isDigit)
import Data.List (isPrefixOf, drop, tails, inits, isInfixOf)
import Data.Maybe (fromMaybe)

removePrefix :: String -> String -> String
removePrefix s x = drop ((length x) - 1) s

goThrough :: [String] -> String -> String
goThrough [] s = goThrough (iterateABiggerString s) s
goThrough [x] s = replaceWithDict wordDigits s -- s == x
goThrough (x:xs) s
    | isReplaceable wordDigits x = goThrough [] ((replaceWithDict wordDigits x) ++ (removePrefix s (head xs)))
    | otherwise = goThrough xs s

iterateABiggerString :: String -> [String] -- ["a", "ab", "abc", "abcd"] from "abcd"
iterateABiggerString s = map (takeWhile (/= ' ')) $ tail $ inits s

replaceWithDict :: [(String, String)] -> String -> String
replaceWithDict [] s = s
replaceWithDict ((k,v):xs) s = replaceWithDict xs (replace k v s)

isReplaceable :: [(String, String)] -> String -> Bool
isReplaceable [] _ = False
isReplaceable ((k,v):xs) s
    | k `isInfixOf` s = True
    | otherwise = isReplaceable xs s

replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new s@(x:xs)
    | old `isPrefixOf` s = new ++ replace old new (drop (length old) s)
    | otherwise = x : replace old new xs

wordDigits :: [(String, String)]
wordDigits = [("one", "o1e"), ("two", "t2o"), ("three", "t3e"), ("four", "f4r"), ("five", "f5e"), ("six", "s6x"), ("seven", "s7n"), ("eight", "e8t"), ("nine", "n9e")]

onlyDigits :: String -> String
onlyDigits = filter isDigit

shell :: String -> [Char]
shell [] = [] -- looking though my inputs, I don't think this is necessary
shell [x] = [x,x] -- for the case of a single digits
shell (x:y:[]) = [x,y] -- for the case of two digits
shell (x:xs) = [x, last xs] -- rest of the cases

toNumber :: String -> Int
toNumber = read

sumAll :: [Int] -> Int
sumAll [] = 0
sumAll (x:xs) = x + sumAll xs

main :: IO ()
main = do
    input <- readFile "../data/input4.txt"
    let content = lines input
    -- print content
    let values = map (goThrough []) content -- you only need "goThrough" here in the second part of the puzzle
    
    -- 1. filter out all non-digits
    -- 2. take only the first and last digit
        -- 2.1 if there is only one digit, repeat it
    -- 3. convert to number
    -- 4. sum all numbers

    -- 1-4
    -- print values
    let result = map toNumber $ map shell $ map onlyDigits values
    -- print result

    -- zip content values result and print each value side by side
    -- print each tuple in a new line
    mapM_ print $ zip3 content values result

    print $ sumAll result