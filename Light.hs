
import Data.List

generator1 :: [(Int,Int,Int,Int)]
generator1 = [(hr,mn,dy,mt)
    | hr <- [0..23]
    , mn <- [0..59]
    , mt <- [1..12]
    , let days = daysOfMonth mt
    , dy <- [1..days]
    ]

daysOfMonth :: Int -> Int
daysOfMonth = memoize daysOfMonth'
    where
    daysOfMonth' 2 = 28
    daysOfMonth' m
      | m `elem` [4, 6, 9, 11] = 30
      | otherwise = 31

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

magic :: (Int,Int,Int,Int) -> Bool
magic (hr,mn,dy,mt) = 
    prime (segments(hr,mn,dy,mt))
    && nodups ( foldr (++) [] [(digits hr) ++ (digits mn) ++ (digits dy) ++ (digits mt)] )

prime :: Int -> Bool
prime = 
    not . factorable 2

factorable :: Int -> Int -> Bool
factorable f n
    | f*f <= n = n `mod` f == 0 || factorable (f+1) n
    | otherwise = False

digits :: Int -> [Int]
digits x  
    | x < 10 = [0, x] 
    | otherwise = [x `div` 10, x `mod` 10]


nodups :: [Int] -> Bool
nodups = nodups' . sort
  where
    nodups' [] = True
    nodups' [_] = True
    nodups' (x1 : x2 : xs) = x1 /= x2 && nodups' (x2 : xs)

segment :: Int -> Int
segment = ([6, 2, 5, 5, 4, 5, 6, 3, 7, 6] !!)

segments :: (Int,Int,Int,Int) -> Int
segments (hr,mn,dy,mt) =
    sum segmentList
    where
    digitList = digits hr ++ digits mn ++ digits dy ++ digits mt 
    segmentList = map segment digitList

check :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
check (hr,mn,dy,mt) 
    | mn < 60 = (hr,mn,dy,mt) 
    | otherwise = hour(hr+1,mn-60,dy,mt)

hour :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
hour (hr,mn,dy,mt) 
    | hr < 24 = (hr,mn,dy,mt) 
    | otherwise = day(hr-24,mn,dy+1,mt)

day :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
day (hr,mn,dy,mt) 
    | dy <= daysOfMonth mt = (hr,mn,dy,mt) 
    | otherwise = day(hr,mn,dy-(daysOfMonth mt),mt+1)
    
month :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
month (hr,mn,dy,mt) 
    | mt <= 12 = (hr,mn,dy,mt) 
    | otherwise = day(hr,mn,dy,mt-12)

tester1 :: (Int,Int,Int,Int) -> Bool
tester1 (hr,mn,dy,mt) =
    magic t1 
    && magic t2
    && segments t3 == (segments t1 + segments t2) `div` 2 
    where
    t1 = (hr,mn,dy,mt)
    t2 = check(hr,mn,dy+1,mt)
    t3 = check(hr,mn+1,dy+1,mt)

x_tester1 :: Int
x_tester1 =
    length [ t | t <- ts , tester1 t ]
    where
    ts =
      [ ( 6 ,59 ,17 ,24)
      , ( 6 ,59 ,17 ,34)
      , ( 6 ,59 ,27 ,14)
      , ( 6 ,59 ,27 ,41)
      , ( 8 ,59 ,12 ,46)
      , (16 ,59 , 7 ,24)
      , (16 ,59 , 7 ,42)
      , (16 ,59 , 7 ,43)
      , (16 ,59 ,27 ,40)
      , (18 ,59 , 2 ,46)
      ]

main :: IO ()
main =
    print ( filter tester1 generator1 )