{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace

main = do
  putStrLn "What are the dimensions:"
  putStrLn "n: "
  n <- getLine
  putStrLn "m: "
  m <- getLine
  --putStrLn $ "The result is "  ++ show (numPaths (read n :: Int)  (read m :: Int) )
  --putStrLn $ "The result' is " ++ show (numPaths2 (read n :: Int)  (read m :: Int) )
  putStrLn $ "The result' is " ++ show (numPaths3 (read n :: Int)  (read m :: Int) )
  

numPaths :: Int -> Int -> Int
numPaths n m = a (n+1) (m+1)
  where 
    a :: Int -> Int -> Int
    a 1 _ = 1
    a _ 1 = 1
    a i j = a (i-1) j + a i (j-1)

numPaths2 :: Int -> Int -> Int
numPaths2 n m = a !! ((n+1) * (m+1) - 1)
  where a = map f [0..]
        f i
          | i <= (m+1)         = 1
          | i `mod` (m+1) == 0 = 1
          | otherwise          = f (i-1) + f (i - (m+1))

numPaths3 :: Int -> Int -> Int
numPaths3 n m = trace ("a(len" ++ show (length a) ++ ":"++show a++") !!" ++ show sink) $ a !! sink
  where sink = ((n+1) * (m+1) - 1)
        a = trace ("fold using sink: "++show sink) $ foldl (\acc i -> acc++(f acc i):[]) [] $ take (sink+1) [0..]
        f :: [Int] -> Int -> Int
        f acc i
          | i <= (m+1)         = trace (show "f "++ show acc++" "++ show i++": " ++ show i++"<="++show (m+1)) $ 1
          | i `mod` (m+1) == 0 = trace (show "f "++ show acc++" "++ show i++": " ++ show i++"`mod`"++show (m+1)++" == 0") $ 1
          | otherwise          = trace (show "f "++ show acc++" "++ show i++": " ++ show (acc!!(i-1))++" + "++show(acc!!(i-(m+1)))) $ acc!!(i-1) + acc!!(i-(m+1))
