module DataStructures where

	data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show,Eq)

	data LinkedList a = End | Link a (LinkedList a) deriving  Eq

	treeMap :: (a->b)->Tree a -> Tree b

	treeMap f = g where
		g (Leaf a) = Leaf (f a)

	data Date = Date {day::Int,month::Int,year::Int}

	instance Eq Date where
		Date day1 month1 year1 == Date day2 month2 year2 = (day1==day2) && (month1==month2) && (year1==year2)
	instance Show Date where
		show (Date a b c) = (show a) ++ "/"++(show b) ++ "/"++(show c)

	
	listMap f = g where
		g (End) = End
		g (Link a b) = Link (f a) (g b)

	instance (Show a) => Show (LinkedList a) where
		show (End) = "."
		show (Link a b) = show a ++ "," ++ show b