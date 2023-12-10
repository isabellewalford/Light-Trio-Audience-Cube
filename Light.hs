-- works
generator1 :: [(Int,Int,Int,Int)]
generator1 = [(hr,mn,dy,mt)
    | hr <- [0..23]
    , mn <- [0..59]
    , mt <- [1..12]
    , let days = daysOfMonth mt
    , dy <- [1..days]
    ]

-- works
daysOfMonth :: Int -> Int 
daysOfMonth m 
    | m == 2 = 28
    | m `elem` [4,6,9,11] = 30
    | otherwise = 31

tester1 :: (Int,Int,Int,Int) -> Bool
tester1 (hr,mn,dy,mt) =
    magic t1 
    && magic t2
    && segments t3 == (segments t1 + segments t2) `div` 2 
    where
    t1 = (hr,mn,dy,mt)
    t2 = (hr,mn,(dy+1),mt)
    t3 = (hr,mn,(dy+1),(mt+1))

magic :: (Int,Int,Int,Int) -> Bool
magic (hr,mn,dy,mt) = 
    prime (hr+mn+dy+mt)
    && nodups ( foldr (++) [] [(digits 1) ++ (digits 1) ++ (digits 1) ++ (digits 1)] )

-- works
prime :: Int -> Bool
prime = 
    not . factorable 2

-- works
factorable :: Int -> Int -> Bool
factorable f n
    | f*f <= n = n `mod` f == 0 || factorable (f+1) n
    | otherwise = False

-- works 
digits :: Int -> [Int]
digits x  
    | x < 10 = [0] ++ [x] 
    | otherwise = [x `div` 10] ++ [x `mod` 10]


nodups :: [Int] -> Bool
nodups s = 
    s == nub s 

-- all good  
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

-- works
segments :: (Int,Int,Int,Int) -> Int
segments (hr,mn,dy,mt) =
    sum segmentList
    where
    digitList = (digits hr) ++ (digits mn) ++ (digits dy) ++ (digits mt)
    segmentList = map segment digitList
     
main :: IO ()
main =
    print ( filter tester1 generator1 )