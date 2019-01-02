module Program2 where 
takeInt :: Int -> [a] -> [a]
takeInt n (x:xs) = go n (x:xs) 1
 where 
  go n (x:xs) m
   | m==n = [x]
   | True = [x] ++ (go n xs (m+1))

dropInt :: Int -> [a] -> [a]
dropInt n (x:xs) = go n (x:xs) 1
 where 
  go startDrop (x:xs) counter 
   | counter<startDrop = go startDrop xs (counter+1)
   | counter == length (x:xs) = []
   | counter>=startDrop = [x] ++ go startDrop xs (counter+1) 
sumInt :: [Int]->Int
sumInt [] = 0
sumInt (x:xs) = x + sumInt xs
negateEl :: (Num a) => a->a
negateEl x = (-1)*x
negateList = map negateEl

guessNum = do 
	putStrLn "Write a number"
	line <- getLine
	case compare (read line) 12 of
		LT -> do
			putStrLn "Too Low!!"
			guessNum
		GT -> do 
			putStrLn "Too high!!"
			guessNum
		EQ -> do putStrLn "yeah"

(!) n step
	| n>step = n * ( (n-step) ! step)
	| True = n