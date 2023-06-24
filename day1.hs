main = do 
    s <- readFile "day1.input"
    putStrLn . show $ compute s

compute :: String -> Int
compute s = maximum $ processList s 

processList :: String -> [Int]
processList s = processListHelper [] 0 $ lines s

processListHelper :: [Int] -> Int -> [String] -> [Int]

processListHelper numList currentNum [] = numList
processListHelper numList currentNum [""] = currentNum:numList
processListHelper numList currentNum ("": xs) = processListHelper (currentNum:numList) 0 xs
processListHelper numList currentNum (a: xs) = processListHelper numList (currentNum + (read a)) xs
