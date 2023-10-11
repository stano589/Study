import Data.List (transpose)
reduceList :: Int -> [a] -> [a]
reduceList n x
    | null x = []
    | otherwise = take 1 (drop n x) ++ reduceList n (drop (n+1) x)

repeatSkip :: Int -> [a] -> [[a]]
repeatSkip n x
    | n < 0 = []
    | otherwise = reduceList n x : repeatSkip (n-1) x 

skips :: [a] -> [[a]]
skips x = take (length x) (reverse (repeatSkip (length x) x))

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
    | b > a && b > c = b : localMaxima (c:xs)
    | otherwise = localMaxima (b:c:xs)
localMaxima xs
    | length xs < 2 = []
localMaxima _ = []

frequency :: Int -> [Int] -> Int
frequency x = length . filter (x==)

counts :: [Int] -> [Int] -> [Int]
counts vs xs = map (flip frequency xs) vs

generateFreq :: [Int] -> [Int]
generateFreq = counts [0,1,2,3,4,5,6,7,8,9]

horizontalHisto :: [Int] -> [String]
horizontalHisto = map (flip replicate '*') . generateFreq

-- rotate 90 degrees
rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

-- fill to rectangle with space padding
rectangular :: a -> [[a]] -> [[a]]
rectangular padding rows = rectangle where
  width = maximum $ map length rows
  padded = map (++ repeat padding) rows
  rectangle = map (take width) padded

verticalHisto :: [Int] -> [String]
verticalHisto = rotate . rectangular ' ' . horizontalHisto

histogram :: [Int] -> String
histogram [] = "==========\n0123456789\n"
histogram x = unlines( verticalHisto x) ++ histogram []

printHisto = putStr . histogram