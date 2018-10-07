head [1,2,3,4,5]

tail [1,2,3,4,5]

fst (1,True)

snd (1,True)

reverse [1,2,3,4,5]

length [1,2,3,4,5]

True && False

not True

map not [True,False,True]

map (\x -> x * x) [1,2,3,4]

foldl (+) 0 [1,2,3,4]
foldl (\x -> \y -> x + y * y) 0 [10,20,30]

---------------------------

fact n = if n <= 1 then 1 else n * fact(n-1)

fib n = if n < 2 then n else fib(n-1) +fib(n-2)

fact 0 = 1
fact n = n * fact(n-1)

fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

help 0 a b = a
help n a b = help (n-1) b (a+b)
fib n = help n 0 1

fib n = help n 0 1 where
	help 0 a b = a
	help n a b = help (n-1) b (a+b)

fib n = let
	help 0 a b = a
	help n a b = help (n-1) b (a+b)
		in help n 0 1

map' f list = if null list then [ ] else f (head list) : map' f (tail list)

-------------------------

filter' _ [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

curry' f x y = f (x,y)
uncurry' f (x,y) = f x y

mapodd = map odd

m f = map f [1,2,3,-4,-5]

----------------------

foldl (+) 0 [0,1,2,3]

mod 5 3
5 `mod` 3

(^4) `map` [2,3,4,5]

[x * x | x <- [1,2,3,4,5]]

[x * x | x <- [1,2,3,4,5], odd x]

squares xs = [x*x | x<-xs]

qsort (x:xs) = qsort lesser ++ [x] ++ qsort greater
	where
	lesser = [y | y<-xs, y<=x]
	greater = [y | y<-xs, y>x]

[1..10]
[1,3..9]

take 5 [1,3..]
drop 10 [1,3..29]

-----------------------

pairs xs ys = [(x,y) | x<-xs, y<-ys]

zip' xs ys = [(x!!k, y!!k) | k<-[0..n]] where n = min (length xs) length(ys) - 1

factlist = map fact [0..n] where
	fact 0 = 1
	fact n | n > 0 = n * fact(n-1)
take 10 factlist

factlist' = [product [1..n] | n<-[0..]]

factlist'' = 1 : zipWith (*) factlist'' [0..]

fiblist = 0 : 1 : zipWith (+) fiblist (tail fiblist)

fiblist' = help 0 1 where help x y = x : helper y (x+y)

powersOf base = 1 : map (*base) (powersOf base)

primelist = checkdiv [2..] where
	checkdiv (p:xs) = 
		p : checkdiv [x | x<-xs, mod x p /= 0]

---------------------------

data Color = Red | Green | Blue
	deriving (Show, Read, Eq, Ord)

data Shape = Circle Float | Rectangle Float Float
	deriving (Eq, Ord, Show, Read)

area (Circle radius) = pi * radius^2

perimeter (Circle radius) = 2*pi*radius

shapelist = [Circle 10, Rectangle 10 15]

----------------------------

data List a = Nil | Cons a (List a)
	deriving (Eq, Ord, Show, Read)

len Nil = 0
len (cons _ xs) = 1 + len xs

app Nil ys = ys
app (Cons x xs) ys = Cons x (app xs ys)

rev Nil = Nil
rev (Cons x xs) app (rev xs) (Cons x Nil)

list1 = Cons 10 (Cons 20 (Cons 30 Nil))

-------------------------

qualifiers

Eq == /=
Ord < > <= >= max min
Num + - * abs
Integral div mod quot rem
Fractional / recip