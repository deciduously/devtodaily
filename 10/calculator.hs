-- Challenge 10
-- https://dev.to/thepracticaldev/daily-challenge-10-calculator-23n7
-- executes an arithmetic equation represented as a string adhering to order of operations
-- NOTE not true PEMDAS - executes left-to-right but with div and mult as higher precedence.

import Data.Char (isDigit)

data Token = Num Int | Add | Sub | Mul | Div deriving (Eq, Show)

input :: String
input = "2 / 2 + 3 * 4 - 6"

tokenize :: String -> [String]
tokenize s = filter (/= " ") $ map (:[]) s

parse :: [String] -> [Token]
parse [] = []
parse (t:ts)
    | isDigit $ t !! 0 = Num (read t) : parse ts
    | t == "*" = Mul : parse ts
    | t == "/" = Div : parse ts
    | t == "+" = Add : parse ts
    | t == "-" = Sub : parse ts
    | otherwise = parse ts

division :: [Token] -> [Token]
division [] = []
division [a] = [a]
division (t:ts) =
    case t of
        Num x ->
            case head ts of
                Div ->
                    let
                        (Num y) = head $ tail ts
                    in
                        Num (quot x y) : division (tail $ tail ts)
                _ -> t : division ts
        _ -> t : division ts

multiplication :: [Token] -> [Token]
multiplication [] = []
multiplication [a] = [a]
multiplication (t:ts) =
    case t of
        Num x ->
            case head ts of
                Mul ->
                    let
                        (Num y) = head $ tail ts
                    in
                        Num (x * y) : multiplication (tail $ tail ts)
                _ -> t : multiplication ts
        _ -> t : multiplication ts

addition :: [Token] -> [Token]
addition [] = []
addition [a] = [a]
addition (t:ts) =
    case t of
        Num x ->
            case head ts of
                Add ->
                    let
                        (Num y) = head $ tail ts
                    in
                        Num (x + y) : addition (tail $ tail ts)
                _ -> t : addition ts
        _ -> t : addition ts

subtraction :: [Token] -> [Token]
subtraction [] = []
subtraction [a] = [a]
subtraction (t:ts) =
    case t of
        Num x ->
            case head ts of
                Sub ->
                    let
                        (Num y) = head $ tail ts
                    in
                        Num (x - y) : subtraction (tail $ tail ts)
                _ -> t : subtraction ts
        _ -> t : subtraction ts

evalStr :: String -> Int
evalStr s =
    let
        [(Num x)] = subtraction $ addition $ multiplication $ division $ parse $ tokenize s
    in
        x
