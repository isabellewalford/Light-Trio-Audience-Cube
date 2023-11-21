
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
    && nodups ( foldr (++) [] (map digits (hr,mn,dy,mt)))

prime :: Int -> Bool
prime = 
    not . factorable 2

factorable :: Int -> Int -> Bool
factorable f n
    | f*f <= n = n `mod` f == 0 || factorable (f+1) n
    | otherwise = False

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

nodups :: [Int] -> Bool
nodups s = s == nub s 

segments :: (Int,Int,Int,Int) -> Int
segments (hr,mn,dy,mt) =
    sum dig
    where 
        dig = foldr (++) [] (map digits (hr,mn,dy,mt))