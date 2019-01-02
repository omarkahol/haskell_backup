{-# LANGUAGE Arrows #-}

module Main where
	import Control.Arrow
	import Control.Monad
	import qualified Control.Category as Cat
	import Data.List
	import Data.Maybe
	import System.Random

	newtype Circuit a b = Circuit {runCircuit :: a -> (Circuit a b, b)}

	instance Cat.Category Circuit where
		id = Circuit $ \ a -> (Cat.id, a)
		(.) = dott
			where dott (Circuit c2) (Circuit c1) = Circuit $ \ a ->
				let 
					(c1n, br) = c1 a
					(c2n, cr) = c2 br 
				in (dott c2n c1n, cr)

	instance Arrow Circuit where
		arr f = Circuit $ \ a -> (arr f, f a)
		first (Circuit c1) = Circuit $ \ (b,d) -> 
			let (c1', c) = c1 b
			in (first c1', (c,d))

	runArrow :: Circuit a b -> [a] -> [b]
	runArrow cir = snd . mapAccumL runCircuit cir
	
	accum :: acc -> (a -> acc -> (b,acc)) -> Circuit a b
	accum acc f = Circuit $ \ input ->
		let (output, acc')=f input acc
		in (accum acc' f, output)

	accum' :: b -> (a->b -> b) -> Circuit a b
	accum' acc f = accum acc ( \ a b -> let b' = f a b in (b, b'))

	total :: Num a => Circuit a a
	total = accum' 0 (+)


	main = putStrLn "Hello World"
