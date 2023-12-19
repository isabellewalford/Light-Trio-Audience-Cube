import Data.List 

-- Generator

combinations3digit :: [Int]
combinations3digit = [123,124,125,126,127,128,129,
    134,135,136,137,138,139,
    145,146,147,148,149,
    156,157,158,159,
    167,168,169,
    178,179,
    189]

digits :: Int -> [Int]
digits 0
    = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

genTuples :: [Int] -> [([Char], [Char], [Char], [Char], [Char])]
genTuples [] = []
genTuples (x:xs) = [(n1,n2,n3,n4,n5)
    | n1 <- permutations (show x) 
    , n2 <- twoDigitPerms 
    , n3 <- permutations (show x)
    , n4 <- twoDigitPerms
    , n5 <- permutations (show x)
    ] ++ genTuples xs 
    where 
        numList = digits x
        n1 = show (head numList)
        n2 = show (numList !! 1)
        n3 = show (numList !! 2)
        twoDigitPerms = (permutations (n1++n2)) ++ (permutations (n1++n3)) ++ (permutations (n2++n3))

generator2 :: [([Char], [Char], [Char], [Char], [Char])]
generator2 = genTuples combinations3digit

x_generator2 :: Int
x_generator2 =
    length [ t | t <- ts , t `elem` g ]
    where
    g = generator2
    ts =
        [ ("123","21","123","12","123")
        , (" 162","26","261 ","12","621 ")
        , (" 219","19","912 ","21","291 ")
        , (" 329","92","932 ","32","239 ")
        , (" 439","94","394 ","43","394 ")
        , (" 549","95","945 ","95","945 ")
        , (" 568","68","586 ","56","586 ")
        , (" 769","67","679 ","97","796 ")
        , (" 879","79","897 ","98","789 ")
        , (" 987","79","789 ","79","789 ")
        ]

-- Tester

tester2 :: ([Char], [Char], [Char], [Char], [Char]) -> Bool
tester2 (n1, n2, n3, n4, n5) = 
    num1 - num2 == num3
    && num3 - num4 == num5
    && num1 + num3 + num5 < 2000
    where
        num1 = read n1 :: Int
        num2 = read n2 :: Int
        num3 = read n3 :: Int
        num4 = read n4 :: Int
        num5 = read n5 :: Int

x_tester2 :: Int
x_tester2 =
    length [ t | t <- ts , tester2 t ]
    where
    ts =
        [ ("138","01","137","50","87")
        , (" 143","01","142 ","52","90")
        , (" 171","02","169 ","79","90")
        , (" 152","03","149 ","54","95")
        , (" 159","04","155 ","61","94")
        , (" 161","05","156 ","63","93")
        , (" 182","06","176 ","80","96")
        , (" 151","07","144 ","57","87")
        , (" 165","08","157 ","64","93")
        , (" 174","09","165 ","71","94")
        ]


main :: IO ()
main = 
  print  x_generator2