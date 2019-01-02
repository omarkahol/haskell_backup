module State where
import Prelude hiding (read)
import Control.Applicative
import Control.Monad

data S s a = S {runS :: s -> (a, s)}

instance Functor (S s) where
	fmap f (S s) = S $ \ st -> let (a, s') = s st in (f a, s')

instance Applicative (S s) where
 	pure a = S $ \ s -> (a, s)
 	(<*>)=Control.Monad.ap

instance Monad (S s) where
	return = pure
	S f >>= k = 
		S $ \ s -> let (a, s') = f s in 
			runS (k a) s'
fetch :: S s s
fetch = S $ \ s -> (s,s)

store :: s -> S s ()
store x = S $ \ _ -> ((), x)

tick :: S Int Int 
tick = fetch >>= \ n -> (store (n+1) >>= \ _ -> return n)

