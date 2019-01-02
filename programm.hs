module Neperial where
n ! step
 | n<=step = n
 | True = n*((n-step)!step)
nep(0)=1/(1!1)
nep(n)=(1/n!1) + nep(n-1)
e=nep(17)
exceptionzeta(1,s)=1
exceptionzeta(n,s)= 1/(n^s) + exceptionzeta(n-1,s)
zeta(s)=exceptionzeta(1000,s)
pi_greco :: Double
pi_greco=sqrt(6*exceptionzeta(10000,2))
data Sign = Positive|Negative|Null deriving Show
mySignum::Double->Sign
mySignum(x)=
 if x<0
  then Negative
 else if x>0
  then Positive
 else Null
factorial n = go n 1
 where
  go n res
   |n>1 = go(n-1)(res*n)
   |otherwise = res
log2(n)
 | n >2 =1+log2(n/2)
 | otherwise=1
bottom :: [a]->a
bottom [x]=x
bottom (x:xs) = bottom (xs)
replicater :: Int -> a -> [a]
replicater 0 a = []
replicater n a = [a]++replicater (n-1) a

rzip::[a]->[b]->[(a,b)]
rzip [x] [y] = [(x,y)]
rzip (x:xs) (y:ys) = [(x,y)] ++ goer xs ys
 where
  goer xs ys
   |length xs > length ys = rzip (take (length ys) xs) ys
   |length ys > length xs = rzip  xs (take (length xs) ys)
   |otherwise = rzip xs ys

leng :: [a]->Int
leng[]=0
leng (x:xs) = 1 + leng (xs)
elementAt :: Int -> [a] -> a
elementAt n (x:xs) = goer n (x:xs) 1
 where
  goer n (x:xs) m
   | n==m = x
   | True = goer n xs (m+1)
expo x = e^x
applyToIntegers :: (Integer->Double)->[Integer]->[Double]
applyToIntegers _ [] = []
applyToIntegers f (x:xs) = (f x):applyToIntegers f xs
