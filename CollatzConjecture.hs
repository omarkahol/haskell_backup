module CollatzConjecture where
	
	collatz :: Int -> [Int]
	collatz 1 = []
	collatz n =
		if even n 
			then ((div n 2) : collatz (div n 2))
		else ((3*n +1) : collatz (3*n+1))





	

	collatzShow::Int->IO()
	collatzShow n = do
		if n == 1 
			then do
				putStrLn (show n)
			else do
				if even n
					then do 
						putStrLn (show (div n 2))
						collatzShow (div n 2)
				else do
					putStrLn (show (3*n+1))
					collatzShow ( 3*n +1)









					

	data Date = Date Int Int Int deriving (Show,Eq)

	buildDate::Int->Int->Int->Date
	buildDate d m y =
		if d<1 || d>31
			then Date 0 0 0 
		else 
			if m<1 || m>12
				then Date 0 0 0
			else Date d m y

	errorDate :: Date -> Bool
	errorDate d = 
		if d == (Date 0 0 0)
			then False
		else True
