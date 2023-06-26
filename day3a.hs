import Data.Set (Set, intersection, fromList, elemAt)

main :: IO ()
main = do 
    s <- readFile "day3.input"
    putStrLn $ show (solution s)


solution :: String -> Int
solution s = sum . map priority $ overlaps s

-- 'a' through 'z' gets priority 1 through 26, and 'A' through 'Z' gets priority 27 through 52
priority :: Char -> Int
priority c 
    | c `elem` ['a'..'z'] = fromEnum c - (fromEnum 'a' - 1)
    | c `elem` ['A'..'Z'] = fromEnum c - (fromEnum 'A' - 1) + priority 'z'
    | otherwise = error "Non-alphabetical character"

overlap :: String -> Char
overlap s = head $ overlaps s

overlaps :: String -> [Char]
overlaps s =  map parseLine $ lines s


-- Finds a common letter between two halves of a string
parseLine :: String -> Char
parseLine s = elemAt 0 . uncurry intersection . mapPair fromList $ splitAt (div (length s + 1)  2) s

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a1, a2) = (f a1, f a2)

