
data Move = Rock | Paper | Scissors | InvalidInput deriving (Eq, Show)

moveScore :: Move -> Int
moveScore Rock = 1 
moveScore Paper = 2
moveScore Scissors = 3
moveScore InvalidInput = -1000000 --debugging purposes


beats :: Move -> Move
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock


tournamentScore :: String -> Int
tournamentScore s = sum . map roundScore . map parseLine $ lines s


roundScore :: [Move] -> Int
roundScore (oppMove:ourMove:_) = moveScore ourMove + headToHeadScore oppMove ourMove 

headToHeadScore :: Move -> Move -> Int
headToHeadScore x y 
    | y == x = 3 
    | y == beats x = 6 
    | otherwise = 0


parseLine :: String -> [Move]
parseLine s = map parseMove $ words s

parseMove :: String -> Move
parseMove s 
    | s == "A" || s == "X" = Rock
    | s == "B" || s == "Y" = Paper
    | s == "C" || s == "Z" = Scissors 
    | otherwise = InvalidInput -- should never get here! In future, use Maybes 




main :: IO ()
main = do
    input <- readFile "day2.input"
    putStrLn . show $ tournamentScore input



