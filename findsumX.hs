				
qsort :: Ord a => [a] -> [a]
qsort []	= []
qsort (x:xs)	= qsort greater ++ [x] ++ qsort lesser 
	where lesser  = filter (< x)  xs
	      greater = filter (>= x) xs  

findsumX :: Int -> [Int] -> [Int]
findsumX a xs = findsumX' a (qsort xs)

findsumX' :: Int -> [Int] -> [Int]
findsumX' 0 xs 		= []
findsumX' a [] 		= []
findsumX' a (x:xs)
	| a == x	= [x]
	| a < x		= findsumX' a xs
findsumX' a xs		= head $ filter (\x -> sum x == a) [x : findsumX' (a-x) (filterOnce (/=x) xs) | x <- xs]


filterOnce :: Eq a => (a -> Bool) -> [a] -> [a]
filterOnce f [] = []
filterOnce p (x:xs)	
	| p x 		= x : filterOnce p xs
	| otherwise 	= filterOnce (\_ -> True) xs

