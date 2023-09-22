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