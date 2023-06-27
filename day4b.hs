main :: IO ()
main = do
    input <- readFile "day4.input"
    putStrLn . show $ solution input

type Range = (Int, Int)

parseLine :: String -> (Range, Range)
parseLine s = listPair (map parseRange $ splitOn s ',')

parseRange :: String -> Range
parseRange s = listPair $ map read $ splitOn s '-'

--convenience function for converting lists of length two into a 2-tuple
listPair :: [a] -> (a, a)
listPair [x,y] = (x,y)
listPair xs = error "Input is not length two"

splitOn :: String -> Char -> [String]
splitOn [] _ = []
splitOn s c = chunk : (splitOn (drop 1 rest) c)
            where 
                (chunk, rest) = span (/= c) s

overlaps :: (Range, Range) -> Bool
overlaps ( (x1, x2), (y1, y2)) = (x1 <= y1 && y1 <= x2) || (y1 <= x1 && x1 <= y2)

solution :: String -> Int
solution s = length . filter (==True) . map overlaps . map parseLine $ lines s 
