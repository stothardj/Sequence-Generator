module Main where

import System.IO
import Data.List (intersperse)

main :: IO ()
main =
  do
    hSetBuffering stdout NoBuffering
    promptLoop
    
promptLoop =
  do
    putStr "Please specify the type of sequence you wish to generate.\n        a  ---  Arithmetic (linear)\n        p  ---  Polynomial (specify degree)\n        g  ---  Geometric  (exponential)\nEnter 'q' to quit or 'h' to display this menu again.\napgqh> "
    choice <- fmap head getLine
    processChoice choice

processChoice :: Char -> IO ()
processChoice choice
  | choice == 'q' = return ()
  | choice == 'h' = promptLoop
  | choice == 'a' = promptArithmetic
  | choice == 'p' = promptPolynomial
  | choice == 'g' = promptGeometric
  | otherwise = return ()
                
promptArithmetic :: IO ()
promptArithmetic =
  do
    putStr "Arithmetic sequence { a0 + i*d } for i = 0,1,2, ..., L-1.\nEnter the first element a0:                           "
    first <- fmap read getLine :: IO Double
    putStr "Enter the difference d between successive elements:   "
    diff <- fmap read getLine :: IO Double
    putStr "Enter the fragment length L:                          "
    len <- fmap read getLine
    finalOutput [first, first + diff .. first + diff * (len - 1) ]

promptPolynomial :: IO ()
promptPolynomial =
  do
    putStr "Polynomial sequence { c*(x0+i*d)^n } for i = 0,1,2, ..., L-1.\nEnter the degree n:                       "
    n <- fmap read getLine :: IO Double
    putStr "Enter the constant multiplier c:          "
    c <- fmap read getLine :: IO Double
    putStr "Enter the initial value of the base x0:   "
    x0 <- fmap read getLine :: IO Double
    putStr "Enter the base increment d:               "
    d <- fmap read getLine :: IO Double
    putStr "Enter the fragment length L:              "
    l <- fmap read getLine
    finalOutput [ c * (x0 + i * d) ** n | i <- [0..l-1]]

promptGeometric :: IO ()
promptGeometric =
  do
    putStr "Geometric sequence { a0 * r^(i-1) }  for i = 1,2, ..., L.\nEnter the first element a0:                           "
    a0 <- fmap read getLine :: IO Double
    putStr "Enter the ratio r between successive elements:        "
    r <- fmap read getLine :: IO Double
    putStr "Enter the fragment length L:                          "
    l <- fmap read getLine
    finalOutput [ a0 * r ^ (i - 1) | i <- [1..l] ]

finalOutput ls =
  do
    putStrLn "Fragment:"    
    putStrLn $ formattedList ls    
    putStrLn $ "Fragment Sum  " ++ show (sum ls)
    promptLoop
    
formattedList = foldl (\acc x -> acc ++ x) "" . intersperse ",  " . map show