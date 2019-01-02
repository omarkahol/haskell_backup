data Date = European Day Month Year | American Month Day Year
type Day = Int
type Month = Int 
type Year = Int

showDate :: Char -> Date -> String 
showDate sep (European day month year) = show day ++ show sep ++ show month ++ show sep ++ show year
showDate sep (American month day year) = show month ++ show sep ++ show day ++show sep ++ show year
showDateStd = showDate '/'

september = [European x 9 2018 | x <- [1..30], True]
november = [European x 11 2018 | x <-[1..30], True]
year2018 = [European x y 2018 | x <- [1..30], y <-[1..12],True]
showCalendar = map showDateStd


y = let (x:_) = map (*2) [1,2,3]
	in x+5

data Relevance a = NotImportant | Important a
list = take 20 [1..]


relevanceFunction :: [Int] -> [Relevance Int]
relevanceFunction [] = []
relevanceFunction (x:xs) = 
	if (mod x 2) == 0
		then ((Important x) : relevanceFunction xs)
	else
		(NotImportant : relevanceFunction xs)

showRelevance (NotImportant) = "NotImportant"
showRelevance (Important a) = show a 

onlyImportantStuff list = [x | Important x <- list]

strangeF x = if x == 0 then 1 else (sin x)/x

caseF x = case x of 
	0 -> 1
	_ -> (sin x)/x

data Color = RGB Int Int Int deriving Show
colorF c = case c of 
	RGB 0 0 0 -> "Black"
	RGB 255 255 255 -> "White"
	_ -> "Unknown"

stupidProgram = do
	putStrLn "hello what's your name?"
	name <- getLine
	putStrLn ("Hello "++name++". How old are you?")
	age <- getLine
	case compare (read age::Int) 18 of
		LT -> do 
			putStrLn "Sorry you are too young!!"
		_ -> do
			putStrLn "Welcome"

getAngry = do
	return "Hello"
	return "Hey"
	return "I am getting angry!!!!!!"
mainP = do
	s <- getAngry
	putStrLn (s)
	

