module Classes where
	class Located a where 
		getLocation :: a -> (Int,Int)

	class (Located a) => Movable a where
		setLocation :: (Int,Int) -> a -> a

	data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)

	instance Located Point where
		getLocation (Point x y) = (x,y)
	instance Movable Point where
		setLocation (x,y) p = p {x = x, y = y}

	move :: (Movable a)=>(Int,Int)->a->a
	move (dx, dy) p = setLocation (x+dx,y+dy) p
		where (x,y)=getLocation p