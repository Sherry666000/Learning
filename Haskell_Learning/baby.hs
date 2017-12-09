doubleMe x = x + x
doubleUs x y = doubleMe x +doubleMe y
doubleSmallNumber' x = (if x>100 then x else x*2)+1
conanO'Brien = "It's a-me, Conan O'Brien!"
removeNonUppercase st = [c|c<-st, c `elem` ['A'..'Z']]
lucky :: Int -> String
lucky 7 = "Seven"
lucky x = "out of luck"
factorial :: Int->Int
factorial 0=1
factorial n=n* factorial (n-1)
addVectors :: (Double,Double)->(Double,Double)->(Double,Double)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)
head' :: [a]->a
head' []=error"Can't call head on an empty list,dummy"
head' (x:_)=x
tell :: (Show a) => [a] -> String
tell []="The list is empty"
tell (x:[]) = "The list has one element"++show x
tell (x:y:[]) = "The list has two elements:"++show x ++"and"++show y
tell (x:y:_) = "The list is long. The first two elements are:"++show x++"and"++show y
badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x+y+z
firstletter :: String->String
firstletter ""="Empty String,Ops"
firstletter all@(x:xs) = "The first letter of "++all++" is "++[x]
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= 18.5 = "underweight"
    | bmi <= 25 = "normal"
    | bmi <= 30 = "whale"
    where bmi = weight/height ^2
    
max' :: (Ord a) => a ->a ->a
max' a b
   | a < b = b
   | otherwise = a
myCompare :: (Ord a) => a -> a ->Ordering
a `myCompare` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT
nice ::String
nice="nice to"
bad :: String
bad = "Oh"
greet::String->String
greet "Juan" = nice ++ "Juan"
greet "ff" = bad ++ "ff"
greet name = bad ++ " " ++name

initials :: String->String->String
initials firstname lastname= [f] ++ "."++[l]++"."
    where (f:_) = firstname
          (l:_) = lastname
calcBmis :: [(Double, Double)]->[Double]
calcBmis xs = [bmi w h | (w,h)<-xs]
    where bmi weight height = weight / height ^2
cylinder :: Double -> Double -> Double
cylinder r h =  
   let sideArea = 2*pi*r*h
       topArea = pi*r^2
   in sideArea+topArea*2
describeList :: [a]->String
describeList ls ="The list is "++ what ls
   where what [] = "empty"
         what [x] = "single"
	 what xs = "longer"
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
replicate' :: Int -> a -> [a]
replicate' n x
 | n<= 0 = []
 | otherwise = x:replicate' (n-1) x
take' :: (Num i, Ord i)=>i->[a]->[a]
take' n _
 | n <= 0  = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
quicksort :: (Ord a) =>[a]->[a]
quicksort []=[]
quicksort (x:xs) =
    let smallerOrEqual = [a | a<-xs, a <=x]
        larger = [a | a<-xs, a>x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger
multThree :: Int -> Int -> Int -> Int
multThree x y z =x*y*z
divideByTen :: (Floating a)=>a->a
divideByTen = (/10)
applyTwice :: (a->a)->a->a
applyTwice f x =f (f x)
zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y :zipWith' f xs ys
sum' :: (Num a) => [a] -> a
sum' =foldl (+) 0
map' :: (a->b) -> [a]->[b]
map' f xs = foldr(\x acc->f x:acc) [] xs
elem' :: (Eq a)=>a->[a]->Bool
elem' y ys = foldr(\x acc->if x==y then True else acc) False ys
maximum1' :: (Ord a)=>[a]->a
maximum1' = foldl1 max
reverse' :: [a]->[a]
reverse' =foldl (flip(:)) []
product' :: (Num a)=>[a]->a
product' =foldl (*) 1
filter' :: (a->Bool)->[a]->[a]
filter' p=foldr (\x acc->if p x then x:acc else acc) []
and' :: [Bool]->Bool
and' xs = foldr (&&) True xs