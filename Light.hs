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
daysOfMonth m 
    | m == 2 = 28
    | m `elem` [4,6,9,11] = 30
    | otherwise = 31

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
    | x < 10 = [0] ++ [x] 
    | otherwise = [x `div` 10] ++ [x `mod` 10]

nodups :: [Int] -> Bool
nodups s = 
    s == nub s 

segment :: Int -> Int
segment(0) = 6
segment(1) = 2
segment(2) = 5
segment(3) = 5
segment(4) = 4
segment(5) = 5
segment(6) = 6
segment(7) = 3
segment(8) = 7
segment(9) = 6

segments :: (Int,Int,Int,Int) -> Int
segments (hr,mn,dy,mt) =
    sum segmentList
    where
    digitList = digits hr ++ digits mn ++ digits dy ++ digits mt
    segmentList = map segment digitList

check :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
check (hr,mn,dy,mt) 
    | mn < 60 = (hr,mn,dy,mt) 
    | otherwise = hour(hr+1,0,dy,mt)

hour :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
hour (hr,mn,dy,mt) 
    | hr < 24 = (hr,mn,dy,mt) 
    | otherwise = day(0,mn,dy+1,mt)

day :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
day (hr,mn,dy,mt) 
    | dy <= daysOfMonth mt = (hr,mn,dy,mt) 
    | otherwise = day(hr,mn,1,mt+1)
    
month :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
month (hr,mn,dy,mt) 
    | mt < 13 = (hr,mn,dy,mt) 
    | otherwise = day(hr,mn,dy,1)

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