module Main where

import System.IO
import Data.List (intersperse)

data Choices x = List x | Loop | Quit

main :: IO ()
main =
  do
    hSetBuffering stdout NoBuffering
    promptLoop

promptLoop :: IO ()
promptLoop =
  do
    putStr "Please specify the type of sequence you wish to generate.\n        a  ---  Arithmetic (linear)\n        p  ---  Polynomial (specify degree)\n        g  ---  Geometric  (exponential)\nEnter 'q' to quit or 'h' to display this menu again.\napgqh> "
    choice <- fmap head getLine
    let maybels = processChoice choice
    case maybels of
      Quit -> return ()
      Loop -> promptLoop
      List x ->
        do
          ls <- x
          putStrLn "Fragment:"
          putStrLn $ formattedList ls
          putStrLn $ "Fragment Sum  " ++ show (sum ls)
          promptLoop

processChoice :: Char -> Choices (IO [Double])
processChoice choice
  | choice == 'q' = Quit
  | choice == 'h' = Loop
  | choice == 'a' = List promptArithmetic
  | choice == 'p' = List promptPolynomial
  | choice == 'g' = List promptGeometric
  | otherwise = Quit
                
promptArithmetic :: IO [Double]
promptArithmetic =
  do
    putStr "Arithmetic sequence { a0 + i*d } for i = 0,1,2, ..., L-1.\nEnter the first element a0:                           "
    first <- fmap read getLine :: IO Double
    putStr "Enter the difference d between successive elements:   "
    diff <- fmap read getLine :: IO Double
    putStr "Enter the fragment length L:                          "
    len <- fmap read getLine
    return [first, first + diff .. first + diff * (len - 1) ]

promptPolynomial :: IO [Double]
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
    return [ c * (x0 + i * d) ** n | i <- [0..l-1]]

promptGeometric :: IO [Double]
promptGeometric =
  do
    putStr "Geometric sequence { a0 * r^(i-1) }  for i = 1,2, ..., L.\nEnter the first element a0:                           "
    a0 <- fmap read getLine :: IO Double
    putStr "Enter the ratio r between successive elements:        "
    r <- fmap read getLine :: IO Double
    putStr "Enter the fragment length L:                          "
    l <- fmap read getLine
    return [ a0 * r ^ (i - 1) | i <- [1..l] ]

    
formattedList = foldl (\acc x -> acc ++ x) "" . intersperse ",  " . map show