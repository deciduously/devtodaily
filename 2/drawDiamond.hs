-- Draws a formatted diamond
-- https://dev.to/thepracticaldev/daily-challenge-2-string-diamond-21n2

import Data.List (intercalate)

diamond :: Int -> Maybe String
diamond n =
    let
        lines = foldr drawLine [] [0..n]
    in
        if (mod n 2 == 0 || n < 0) then Nothing else Just ((intercalate "\n" lines) ++ "\n")
    where
        drawLine :: Int -> [String] -> [String]
        drawLine m acc =
            let
                midpointTuple = divMod (m - 1) 2
                midpoint = if snd midpointTuple == 1 then (fst midpointTuple) + 1 else (fst midpointTuple)
                paddingSpaces = abs $ (m - midpoint)
                padding = concat $ replicate paddingSpaces " "
                stars = concat $ replicate (n - (paddingSpaces * 2)) "*"
            in
                acc ++ [(padding ++ stars ++ padding)]

showDiamond n =
    case diamond n of
        Just s -> putStr s
        _ -> putStr "Invalid"