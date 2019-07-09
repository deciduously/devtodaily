-- Challenge 11
-- https://dev.to/thepracticaldev/daily-challenge-11-cubic-numbers-21am
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)

failString :: String
failString = "Unlucky"

-- e.g. 153 becomes [1,5,3]
intoDigits :: Int -> [Int]
intoDigits 0 = []
intoDigits n = intoDigits (div n 10) ++ [mod n 10]

-- e.g [1,5,3] becomes 153
fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10 * num + d

-- Primary
-- Ideally, this'd have returned a Maybe String, but spec and all
isCubic :: Int -> String
isCubic n =
    let
        cubes = map (^3) $ intoDigits n
    in
        bool (failString) (show n) (length cubes <= 3 && (sum cubes) == n)

-- Secondary
showCubes :: String -> String
showCubes s =
    let
        maybeDigits = filter (isJust) $ map (\s -> readMaybe s :: Maybe Int) $ words s
        splitLongerThanThrees = map fromDigits $ concat $ map (chunksOf 3) $ map intoDigits $ map fromJust $ maybeDigits
        justCubes = filter (/= failString) $ map isCubic splitLongerThanThrees
    in
        bool (failString) (intercalate " " justCubes) (length justCubes > 0)

-- I added 154 because i think it was supposed to be in the input not the expected output
test1 :: String
test1 = "aqdf& 0 1 xyz 153 154 777.777"

test2 :: String
test2 = "QK29 45[&erui"

test3 :: String
test3 = "24172410 4544 153"