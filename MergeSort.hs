slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

split :: [Int] -> Int -> [[Int]]
split x i
    |   length x <= i = [x]
    |   otherwise = [(slice 0 (i-1) x)] ++ (split (drop i x) i)

cmpSelf :: [Int] -> [Int] -> [Int]
cmpSelf x x2
    |   lx1 == 0 = x2
    |   lx1 == 1 = x2 ++ [head x]
    |   otherwise = 
        if  head x >= (head (drop 1 x))
            then
               (cmpSelf ([head x] ++ (drop 2 x)) x2 ++ [(head (drop 1 x))] )
            else
               (cmpSelf (drop 1 x) x2 ++ [(head x)] )
    where lx1 = length x

cmpSwap :: [Int] -> [Int] -> [Int]
cmpSwap x1 x2
    |   lx1 == 0 && lx2 ==0 = []
    |   lx2 == 0 = x1
    |   lx1 == 0 = x2
    |   (hx1) >= (hx2) = [(hx2)] ++ (cmpSwap (x1) (drop 1 x2))
    --[(hx1),(hx2)] ++
    |   otherwise = [(hx1)] ++  (cmpSwap (drop 1 x1) (x2))
    where lx1 = length x1
          lx2 = length x2
          hx1 = head x1
          hx2 = head x2

merge :: [[Int]] -> [Int]
merge [[]] = []
merge [] = []
merge x
    |   length x == 1 = head x
    |   otherwise = (cmpSwap x1 x2) ++ (merge (drop 2 x))
    where x1 = 
             if length x >= 1
                then
                    
                    (head x) 
                else
                    []
          x2 = 
             if length x >= 2
                then
                    
                     (head (drop 1 x))
                else
                    []

mSort :: [Int] -> Int -> [Int]
mSort x i 
    |  i <= 0 = mSort x 1
    |  i >= length x = x
    |  otherwise = mSort ( merge ( split x i ) ) (i+1) 
