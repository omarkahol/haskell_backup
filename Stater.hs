module ZeroFinder where
	import Control.Monad.Trans.State

	clock :: Double -> State Int Double
	clock r = state $ \ s -> (r, s+1)

	fzero :: Double -> (Double->Double)->Int->Double->State Int Double
	fzero x0 f itmax tol = 
		if itmax > 0 
			then
				if (abs $ x0 - (f x0)) > tol 
					then do
						clock $ x0 - (f x0)
						fzero (x0 - (f x0)) f (itmax - 1) tol 
				else clock $ x0 - (f x0)
		else clock x0

	h = 1e-5 :: Double

	df :: (Double -> Double) -> Double -> Double
	df f x0 = ((f $ x0+h) - (f $ x0-h)) / (2*h)

	fzeroN :: Double -> (Double->Double)->Int->Double->State Int Double
	fzeroN x0 f itmax tol = 
		if itmax > 0
			then do
				if (abs $ x0 - (f x0)) > tol 
					then do
						let dfx0 = df f x0
						let diff = (f x0)/dfx0
						clock $ x0 - diff
						fzeroN (x0 - (f x0)) f (itmax - 1) tol
				else clock $ x0 - ((f x0)/(df f x0))
		else clock x0



