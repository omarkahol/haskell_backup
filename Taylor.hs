module Taylor where
	newtype Series = Series {evalSeries :: (Double->Double) -> Double -> [Double]}
	df :: (Double -> Double) -> (Double -> Double)

	df f = \ x -> ((f $ x + 1e-5) - (f $ x -1e-5))/2e-5

	denom = ((\ x -> product [2..x]) <$> [1..])

	taylor :: Series
	taylor = Series $ \ f x0 -> zipWith (*) (((df f $ 0)/) <$> denom) ((x0**) <$> [1..])