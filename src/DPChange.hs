{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace

{-
CODE CHALLENGE: Solve the Change Problem. The DPCHANGE pseudocode is reproduced below for your convenience.
     Input: An integer money and an array Coins = (coin1, ..., coind).
     Output: The minimum number of coins with denominations Coins that changes money.

Sample Input:
     40
     50,25,20,10,5,1

Sample Output:
     2

    DPCHANGE(money, Coins)
     MinNumCoins(0) ← 0
     for m ← 1 to money
        MinNumCoins(m) ← ∞
            for i ← 1 to |Coins|
                if m ≥ coini
                    if MinNumCoins(m - coini) + 1 < MinNumCoins(m)
                        MinNumCoins(m) ← MinNumCoins(m - coini) + 1
    output MinNumCoins(money)
 -}

main = do
  putStrLn "Enter the amount?"
  n <- getLine
  putStrLn "Enter the coins"
  sCoins <- getLine
  --let coins = map read $ words sCoins :: [Int]
  let coins = map read $ wordsWhen (==',') sCoins :: [Int]
      selectedCoins = dpChange coins (read n :: Int)
  putStrLn $ "The result' is " ++ show selectedCoins
  putStrLn $ show $ length selectedCoins

dpChange :: [Int] -> Int -> [Int]
dpChange coins amount = loop coins [] amount
  where loop :: [Int] -> [Int] -> Int -> [Int]
        loop coins selectedCoins amount
          | amount == 0 = selectedCoins
          | otherwise   =
              let (selectedCoin, coins') = selectCoin coins amount
                  remAmount              = amount - selectedCoin
              in  loop coins' (selectedCoin:selectedCoins) remAmount

selectCoin :: [Int] -> Int -> (Int, [Int])
selectCoin allCoins@(c:coins) amount
  | c < amount   = (c, allCoins)
  | c == amount  = (c, coins)
  | otherwise    = selectCoin coins amount
  

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'
