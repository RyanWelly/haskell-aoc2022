import Data.Set (Set, intersection, fromList, elemAt)
main :: IO ()
main = do 
    s <- readFile "day3.input"
    putStrLn . show $ solution s 
    

-- 'a' through 'z' gets priority 1 through 26, and 'A' through 'Z' gets priority 27 through 52
priority :: Char -> Int
priority c 
    | c `elem` ['a'..'z'] = fromEnum c - (fromEnum 'a' - 1)
    | c `elem` ['A'..'Z'] = fromEnum c - (fromEnum 'A' - 1) + priority 'z'
    | otherwise = error "Non-alphabetical character"

--helper function to divide a list into chunks of n (or less for the tail end)
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = (take n xs) : (chunkList n (drop n xs))

commonBadge :: [String] -> Char
commonBadge xs = elemAt 0 (foldl1 intersection (map fromList xs))

solution :: String -> Int
solution s = sum . map (priority . commonBadge) . chunkList 3 $ lines s
