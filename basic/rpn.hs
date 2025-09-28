import Data.List

operations =
  [ ("+", (+)),
    ("-", (-)),
    ("*", (*)),
    ("/", (/))
  ]

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "*" = (x * y) : ys
    foldingFunction (x : y : ys) "+" = (x + y) : ys
    foldingFunction (x : y : ys) "-" = (x - y) : ys
    foldingFunction xs numStr = read numStr : xs
