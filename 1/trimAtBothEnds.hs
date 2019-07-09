-- Removes the first and lest letter of a string over 2 characters long
-- https://dev.to/thepracticaldev/daily-challenge-1-string-peeler-4nep

trimAtBothEnds :: String -> Maybe String
trimAtBothEnds s = if ((length s) <= 2) then Nothing else Just (init.tail $ s)