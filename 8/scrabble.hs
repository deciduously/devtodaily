import Data.Map (Map, (!))
import qualified Data.Map as Map

scores :: Map Char Int
scores = Map.fromList pairs
    where
        pairs = [
            ('a', 1),
            ('b', 3),
            ('c', 3),
            ('d', 2),
            ('e', 1),
            ('f', 4),
            ('g', 2),
            ('h', 4),
            ('i', 1),
            ('j', 8),
            ('k', 5),
            ('l', 1),
            ('m', 3),
            ('n', 1),
            ('o', 1),
            ('p', 3),
            ('q', 10),
            ('r', 1),
            ('s', 1),
            ('t', 1),
            ('u', 1),
            ('v', 4),
            ('w', 4),
            ('x', 8),
            ('y', 4),
            ('z', 10)]

scoreWord :: String -> Int
scoreWord w =
    let
        sevenLetterBonus = if (length $ stripMarkers w) == 7 then 50 else 0
        wordMultiplier =
            let
                suffix = dropWhile (/= '(') w
            in
                if length suffix > 0 then
                    case suffix !! 1 of
                        't' -> 3
                        'd' -> 2
                        _ -> 1
                else 1
        -- 
        preparedWord = expandMarkers $ takeWhile (/= '(') w
        rawScore = sum $ scoreLetters $ preparedWord
    in
        rawScore * wordMultiplier + sevenLetterBonus
    where
        scoreLetters cs = map (\c -> scores ! c) cs

-- transform doubles, triples, carats
-- if we hit an asterisk, replace it with the previous letter
-- if we hit a carat, drop the previus letter
expandMarkers :: String -> String
expandMarkers [] = []
expandMarkers (c:[]) = [c]
expandMarkers (c:rest) =
    case head rest of
        '*' ->
            if (head $ tail rest) == '*' then
                [c] ++ [c] ++ [c] ++ (expandMarkers $ drop 2 rest) else
                [c] ++ [c] ++ (expandMarkers $ tail rest)
        '^' -> expandMarkers $ tail rest
        _ -> [c] ++ expandMarkers rest

-- remove suffix and all markers for deciding on the 7-letter bonus
stripMarkers :: String -> String
stripMarkers w = filter (\c -> c /= '*' && c /= '^') $ takeWhile (/= '(') w
