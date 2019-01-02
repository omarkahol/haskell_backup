findSol :: (Ord a, Floating a) => a->a->a->[a]
findSol a b c =
	let 
		delta = sqrt((b^2)-4*a*c)
		twiceA = 2*a
	in 
		if delta >= 0
			then [(-b-delta)/twiceA, (-b+delta)/twiceA]
		else []

rz x = 
	if x<1
		then 0.0
	else go 100000 x
		where go n s 
			| n==1 = 1/(1^s)
			| True = 1/(n^s) + go (n-1) s

lengthr :: [a] -> Int 
lengthr [] = 0
lengthr (x:xs) = 1+ lengthr xs


concatr :: [a]->[a]->[a]
concatr [] ys = ys
concatr (x:xs) ys = x : concatr xs ys

main = do 
	putStrLn "What is your Name"
	name <- getLine
	putStrLn ("Hello " ++ name)

doGuessing = do
	putStrLn "Guess the number"
	num <- getLine
	if (read num)<12 
		then do 
			putStrLn "Too low"
			doGuessing
	else if (read num)>12
		then do 
			putStrLn "Too high"
			doGuessing
	else do 
		putStrLn "You win!!"

indexAt :: [a] -> Int -> a
indexAt (x:xs) 0 = x
indexAt (x:xs) n = indexAt xs (n-1)

applyToList :: (Num a) => (a->a)->[a]->[a] 
applyToList _ [] = []
applyToList f (x:xs) = f(x) : applyToList f xs

takeSomething :: [a]->Int->[a]
takeSomething (x:xs) 0 = []
takeSomething (x:xs) n = x : takeSomething xs (n-1)

dropSomething :: [a] -> Int -> [a]
dropSomething (x:xs) 0 = (x:xs)
dropSomething (x:xs) n = dropSomething xs (n-1)

andList :: [Bool] -> Bool
andList [] = True
andList (x:xs) = 
	if x==True
		then andList xs
		else False

andListFoldr :: [Bool] -> Bool
andListFoldr = foldr (&&) True 

divisor:: Int -> [Int]

divisor n = filter (\ x -> (mod n x) == 0) [1..n]
allDivisors = map divisor [1..]

evenNumbers :: [Int]
evenNumbers = [n | n <- [1..], mod n 2 == 0]

tupleGenerator n = go 0 n
	where go p n=
		if p>=n
			then [(n,n)]
		else [(p,p)] ++ go (p+1) n

tupleGeneratorBetter 0 = [(0,0)]
tupleGeneratorBetter n = [(n,n)] ++ tupleGeneratorBetter (n-1)

allTuples = go 0 
 	where go n = [(n,n)] ++ go (n+1)

onlyEvenTuples = [ (x,y) | (x,y) <- allTuples,( mod x 2 == 0) && (mod y 2) == 0]

allPairs cond n = [(x,y) | x <- [1..], y <-[1..n], cond]