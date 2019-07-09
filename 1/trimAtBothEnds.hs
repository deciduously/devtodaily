trimAtBothEnds :: String -> Maybe String
trimAtBothEnds s = if ((length s) <= 2) then Nothing else Just (init.tail $ s)