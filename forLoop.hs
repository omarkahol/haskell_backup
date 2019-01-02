module ForLoop where
	for :: a -> (a -> Bool) -> (a->a) -> IO() -> IO()
	for i c f j = do 
		if (c i) == True 
			then do
				j
				for (f i) c f j
		else do
			return ()