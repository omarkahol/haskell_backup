module MonadT where
	import Control.Monad
	import Control.Monad.Trans
	import Control.Monad.Trans.Maybe
	import Data.Char

	isValid :: String -> Bool
	isValid s = length s > 6 && any (\ c -> let n = ord c in n >= 65 && n <= 90) s

	getPassowrd :: MaybeT IO String
	getPassowrd = do
		lift $ putStrLn "Please type password"
		s <- lift getLine
		if isValid s == True 
			then return s
		else do
			lift $ putStrLn "Password must contain at least an upper case char and must be longer than 6"
			getPassowrd