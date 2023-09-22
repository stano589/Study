-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, c)] ++ hanoi (n-1) b a c

main = do
      putStrLn "enter value for x: "
      input1 <- getLine
      let x = (read input1 :: Integer)
      print (hanoi x "a" "b" "c")