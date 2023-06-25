data Move = Rock | Paper | Scissors | InvalidMove deriving (Eq, Show)
data RequiredResult = Win | Loss | Draw | InvalidResult deriving (Eq, Show)

moveScore :: Move -> Int
moveScore Rock = 1 
moveScore Paper = 2
moveScore Scissors = 3
moveScore InvalidMove = -1000000 --debugging purposes

beats :: Move -> Move
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

loses :: Move -> Move
loses Paper = Rock
loses Rock = Scissors
loses Scissors = Paper


parseMove :: String -> (Move, String)
parseMove (s:' ':xs) 
    | s == 'A' = (Rock, xs) 
    | s == 'B' = (Paper, xs)
    | s == 'C' = (Scissors, xs)
    | otherwise = (InvalidMove, xs) -- should never get here! In future, use Maybes 

parseResult :: String -> RequiredResult
parseResult r
    | r == "X" = Loss
    | r == "Y" = Draw
    | r == "Z" = Win
    | otherwise = InvalidResult

parseLine :: String -> (Move, RequiredResult)
parseLine s = (move, result)
            where 
                (move, rest) = parseMove s 
                result = parseResult rest

winScore = 6
drawScore = 3
loseScore = 0

roundScore :: (Move, RequiredResult) -> Int
roundScore (move, Draw) =  moveScore move + drawScore
roundScore (move, Win) = moveScore (beats move) +  winScore
roundScore (move, Loss) = moveScore (loses move) +  loseScore

calculateSolution :: String -> Int
calculateSolution s = sum . map (roundScore . parseLine) $ lines s

main :: IO ()
main = do 
    s <- readFile "day2.input"
    putStrLn . show $ calculateSolution s 
