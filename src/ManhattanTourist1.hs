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

{-
 Solution which build up a one-dimensional array based on the
 formula: a i j = a!!(i-1)*(n+1)+j

 This is - so far - the best solution (without any magic tricks) because each element is only
 calculated once (in contrast to the above solutions where a_ij elements are re-calculated each time

 It looks a bit ugly with all the traces but it can be useful to see what's happening...

 Note: there is a limit to the dimensions (on my computer it's around 57) after which the values
       are no longer valid
 -}
numPaths3 :: Int -> Int -> Int
numPaths3 n m =
  --trace ("a(len" ++ show (length a) ++ ":"++show a++") !!" ++ show sink) $
  a !! sink
  where sink = ((n+1) * (m+1) - 1)
        a = --trace ("fold using sink: "++show sink) $
            foldl (\acc i -> acc++(f acc i):[]) [] $ take (sink+1) [0..]
        f acc i
          | i <= (m+1)         = --trace (show "f "++ show acc++" "++ show i++": " ++ show i++"<="++show (m+1)) $
                                 1
          | i `mod` (m+1) == 0 = --trace (show "f "++ show acc++" "++ show i++": " ++ show i++"`mod`"++show (m+1)++" == 0") $
                                 1
          | otherwise          = --trace (show "f "++ show acc++" "++ show i++": " ++ show (acc!!(i-1))++" + "++show(acc!!(i-(m+1)))) $
                                 acc!!(i-1) + acc!!(i-(m+1))
