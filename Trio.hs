import Data.List 

generate3digit :: [Int]
generate3digit = filter noZero [111..999]

noZero :: Int -> Bool
noZero n = '0' `notElem` show n

digits :: Int -> [Int]
digits 0
    = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

number :: [Int] -> Int
number [] = 0
number (x:xs) 
    = x * 10 ^ (length (x:xs) -1) + number xs

genTuples :: Int -> [(Int,Int,Int,Int,Int)]
genTuples x = [(n1,n2,n3,n4,n5)
    | n1 <- map read (permutations (show x)) :: [Int]
    , n2 <- map number 2digitperms 
    , n3 <- map read (permutations (show x)) :: [Int]
    , n4 <- map number 2digitperms
    , n5 <- map read (permutations (show x)) :: [Int]
    ] 
    where 
        numList = digits n
        2digitPerms = addListsOfLists (permutations [head numList, numList !! 1]) (permutations [tail numList]) (permutations [head numList, numList !! 2])   



-- Example usage
main :: IO ()
main = 
  print  (take 100 generator3)


x_generator2 :: Int
x_generator2 =
    length [ t | t <- ts , t ‘elem ‘ g ]
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