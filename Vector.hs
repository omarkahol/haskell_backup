module Vector where
	import qualified Control.Category as Cat
	import Control.Arrow

	newtype Function a b = Function {runFunction :: a->b}

	instance Cat.Category Function where
		(.) (Function g) (Function f) = Function $ g . f
		id = Function id

	instance Arrow Function where
		arr f = Function f
		first (Function f) = Function $ \ (x,y) -> (f x, y)
