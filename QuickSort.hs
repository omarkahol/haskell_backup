import Data.Char
quicksort::(Ord a)=>[a]->[a]
--base case--> empty list
quicksort [] = []
--recursive definition ahead
quicksort (x:xs) = (quicksort less) ++ (x:equal) ++ (quicksort more) 
	where 
		less =  filter (<x) xs  --[l | l <- xs, l<x]
		equal = filter (==x) xs --[e | e <- xs,e==x]
		more =  filter (>x) xs  --[m | m <- xs, m>x]

quicksortpp::(Ord a)=>(a->a->Ordering)->[a]->[a]
quicksortpp _ [] = []
quicksortpp c (x:xs) = (quicksortpp c less) ++ (x:equal) ++ (quicksortpp c more)
	where
		less = filter (\ y -> c y x == LT) xs
		equal = filter (\ y -> c y x == EQ) xs
		more = filter (\ y -> c y x == GT) xs
compareIgnoreCase x y = compare (if ( ord (x!!0) > 64 && ord (x!!0) < 97) then chr(ord(x!!0)+32) else (x!!0)) (if ( ord (y!!0) > 64 && ord (y!!0) < 97) then chr(ord(y!!0)+32) else (y!!0))
descendingCompare = flip compare
	

