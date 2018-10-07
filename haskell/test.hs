-- fall 2018
-- 1.
-- last el
-- init (all but first)
last' [] = []
last' xs = head (reverse xs)

-- 1 2 3 4 -> 1 2 3
init' [] = []
init' xs = reverse (tail (reverse xs))

-- 2.
-- antitranspose
-- abc
-- def
-- ghi
-- kmn

-- nmk
-- ihg
-- fed
-- cba
--[reverse x | x<-reverse xs]

-- njfc
-- mheb
-- kgda
transpose ([]:_) = []
transpose m = if null (head m) then [] else map head m : transpose (map tail m)
antitranspose m = transpose [reverse row | row<-reverse m]

-- 3.
-- (apply-left funlist x) and (apply-right funlist x) 
-- successively apply all the functions in the funlist beginning with value x.
-- Example: Suppose inc is (lambda (x) (+ x 1)) and double 
-- is (lambda (x) (* x 2)). Then (apply-left (list inc double square) 5) 
-- returns ((5 + 1) * 2)2 = 144. Also (apply-right (list inc double square) 5) 
-- returns ((5**2) * 2) + 1 = 51. [20 points]

applyleft [] x = x
applyleft (y:ys) x = applyleft ys (y x)

applyright [] x = x
applyright funlist x = applyleft (reverse funlist) x
-- applyleft [(+1), (*2)] 2

-- 4. first non null item in nested list list

-- 5.
-- max depth paren 
-- maxdep [] = 1
-- maxdep _ = 0
-- maxdep (x:xs) = 1 + (max (maxdep x) (maxdep xs))

------------------------
--fall 2009 exam 1

-- 1. median (x,y,z) returns the median or central value of x,y,z. Technically,
--  this is the value w such that at least two of x,y,z are   w and at least
--   two of x,y,z are   w. If x,y,z are distinct, then w is the value that 
--   is neither the minimum nor the maximum.

median x y z    | x <= y && x >= z = x
                | x <= z && x >= y = x
                | y <= x && y >= z = y
                | y <= z && y >= x = y
                | z <= y && z >= x = z
                | z <= x && z >= y = z

median x y z =  if x <= y && x >= z then x else
                if x <= z && x >= y then x else
                if y <= z && y >= x then y else
                if y <= x && y >= z then y else
                if z <= x && z >= y then z else
                if z <= y && x >= x then z else -1

-- 2. expand n returns a list of the digits of n in reverse order. 
-- For example, expand 3467 returns [7,6,4,3].

expand 0 = []
expand n = [mod n 10] ++ expand (div n 10)
-- expand n | n>0 = mod n 10 : expand (div n 10)

-- 3. contract is the inverse function of expand. 
-- For example, contract [7,6,4,3] returns 3467.

contract [] = 0
contract x = helper x 0
helper [] _ = 0
helper (x:xs) i = (x*(10^i)) + (helper xs (i+1))

-- 4. appendAll xs appends together all the elements of xs. 
-- For example, appendAll [”abc”,”1234”,”wxyz”,”789”] returns ”abc1234wxyz789”.
--  Do not call the built-in concat function because it does the same thing.

appendAll x = foldl (++) [] x


-- 5. fasten xs ys returns a list of pairs of the corresponding values from xs and ys. 
--  For example, fasten [1,2,3,4] [5,6,7,8] returns [(1,5),(2,6),(3,7),(4,8)].
--   Do not call the built- in zip function because it does the same thing.

-- [a] -> [b] -> [(a,b)]
fasten xs ys = [(xs!!i,ys!!i) | i <- take (length xs) [0..]]

-- fasten [] [] = []
-- fasten (x:xs) (y:ys) = [(x, y)] ++ fasten xs ys

-- 6. unfasten does essentially the opposite of fasten. 
-- For example, unfasten [(1,5),(2,6),(3,7),(4,8)] returns ([1,2,3,4],[5,6,7,8]).
--  Do not call the built-in unzip function because it does the same thing.

-- [(a,b)] -> ([a],[b])
unfasten tups = ([ fst x | x<-tups ], [ snd x | x<-tups ])

-- 7. applyAll fs x applies each function in fs to value x.
--  For example, if increment x = x+1
-- double x = 2*x
-- square x = x*x
-- then applyAll [increment,double,square] 5 returns [6,10,25].

increment x = x+1
double x = 2*x
square x = x*x
-- [t -> b] -> t -> [b]
applyAll fs x = map (\f -> f x) fs

-- 8. applyEach fs xs applies each function in fs to the corresponding value in xs.
--  For example, applyEach [increment,double,square] [5,10,7] returns [6,20,49]

-- [a->b] -> [a] -> [b]
applyEach xs ys = [ (xs!!i) (ys!!i) | i<-take (length xs) [0..]]

-- 9. sumSquareOdds xs returns the sum of the squares of the odd values in xs.
--  For example, sumSquareOdds [1,2,4,5,7,8,10,11] returns 12 + 52 + 72 + 112 = 196.

sumSquareOdds :: Integral a => [a] -> a
sumSquareOdds xs = foldl (+) 0 [x*x | x<-xs, mod x 2 == 1]
-- sumSquareOdds xs = foldl (+) 0 [x*x | x<-xs, odd x]

-- 10. backward xs reverses the order of the elements in xs. 
-- It does the same thing as the built-in function reverse, so do not call reverse. 
-- For example, backward ”abcde” returns ”edcba”.

backward :: [a] -> [a]
backward [] = []
backward (x:xs) = backward xs ++ [x]


-- 11. repeatEach xs repeats each element of xs.
--  For example, repeatEach ”abcde” returns ”aabbccddee”.

repeatEach :: [a] -> [a]
repeatEach [] = []
repeatEach (x:xs) = x:x : repeatEach xs


-- 12. isPrime n returns True iff n is a prime number. 
-- Recall that a prime number has no divisors other than 1 and itself.
-- The smallest prime numbers are 2, 3, 5, 7, 11, 13, 17, ...

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
-- isPrime n = [2 .. floor (sqrt n)]

-- [(mod 49 i) /= 0 | i<-[2 .. floor (sqrt 49)]]

-------------------------------------------
-- fall 2009 exam 2

-- 1. display n constructs the list [[1],[1,2],[1,2,3],...,[1,2,3,...,n]].
--  Example: display 5 returns [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]].

display :: Integral a => a -> [[a]]
display n = [[1..i] | i<-[1..n]]


-- 2. applyEvery fs xs applies every function in fs to every value in xs,
--  and arranges the results into a list with sublists as follows.
--   Example: applyEvery [(+1),(*2),(^2)] [3,4,5,6]
--    returns [[4,5,6,7],[6,8,10,12],[9,16,25,36]].

applyEvery :: [a->b] -> [a] -> [[b]]
applyEvery fs xs = map (\x -> map x xs) fs


-- 3. concatReverse xs does the same thing as concat (reverse xs).
--  However, do not call either concat or reverse.
--   Example: concatReverse ["abc","defg","hij"] returns "hijdefgabc".

concatReverse :: [[a]] -> [a]
reverse' xs = foldl (\x y -> [y] ++ x) [] xs
concatReverse xs = foldl (++) [] (reverse' xs)


-- 4. selectInRange low high xs returns a list of the values in xs that are between
--  low and high, inclusively. 
--  Example: selectInRange 5 9 [1,9,3,7,11,5,13] returns [9,7,5].

-- selectInRange :: Ordinal a => a -> a -> [a] -> [a]
selectInRange low high xs = [x | x<-xs, x >= low && x <= high]


-- 5. sumOfProducts xss returns the sum of the products of the sublists of xss.
--  However, do not call either sum or product.
--   Example: sumOfProducts [[1,2,3,4],[5,6],[7,8],[9]]
--    returns 1*2*3*4+5*6+7*8+9 = 119.

sumOfProducts :: Num a => [[a]] -> a
sumOfProducts xss = foldl (\x y -> x + (foldl (*) 1 y)) 0 xss


-- 6. dotProduct f g id xs ys returns the dot product of lists xs and ys
--  with respect to binary functions f and g. 
--  Assume that xs and ys have the same length. 
--  If xs and ys are empty, return the value id. 
--  Example: dotProduct (+) (*) 0 [2,3,4] [5,6,7] returns 2*5+3*6+4*7 = 56.

dotProduct _ _ id [] [] = id
dotProduct f g id xs ys = foldl f id (zipWith g xs ys)


-- 7. facts = [1,1,2,6,24,120,720,5040,40320,362880,...] is the list of factorials.
--  0!=1, 1!=1, 2!=2, 3!=6, 4!=24, etc.

factorial 0 = 1
factorial x =  x * factorial (x-1)
facts = [ factorial x | x<-take 5 [0..]]

-- 8. fibs = [0,1,1,2,3,5,8,13,21,34,55,...] is the list of Fibonacci numbers. 
-- Each value is the sum of the preceding two values, so for example,
--  the next value not shown is 34+55 = 89.

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
fibs = [fib x | x<-take 5 [0..]]

-- 9. table f xs ys applies binary function f to each value in xs paired with each 
-- value in ys, and arranges the results into a list with sublists as follows. 
-- Assume that xs and ys are infinite lists.
--  Example: table (*) [1..] [1..] returns the infinite
-- multiplication table 
-- [[1,2,3,4,...],[2,4,6,8,...],[3,6,9,12,...],[4,8,12,16,...],...].

table :: (t -> a -> b) -> [t] -> [a] -> [[b]]
table f xs ys = [map (f x) ys | x<-xs]


-- 10. ExtendedFloat can hold any built-in Float value, or positive infinity 
-- or negative infinity. All comparison operators (==, /=, <, <=, >, >=) should 
-- work properly. Function negate :: ExtendedFloat -> ExtendedFloat extends the
--  concept of the unary minus operator to ExtendedFloat values.














