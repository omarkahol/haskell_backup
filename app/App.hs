module App where 

greet :: IO()
greet = do
 name <- putStr "Type your name--> " >> (\ s -> "Hello " ++ s ++ "!!!") <$> getLine
 putStrLn name
