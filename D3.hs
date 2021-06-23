main :: IO ()
main = do
  contents <- readFile "D3_input.txt"
  let trees = lines contents
  let cnt = count trees (1, 3) 0 0
  let slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  let allValues = [count trees slope 0 0 | slope <- slopes]

  putStr "part 1: "
  print cnt

  putStr "part 2: "
  print $ product allValues

count :: [[Char]] -> (Int, Int) -> Int -> Int -> Int
count trees (rd, cd) row col
  | row < length trees =
    let v = if trees !! row !! col == '#' then 1 else 0
     in v + count trees (rd, cd) (row + rd) (mod (col + cd) (length (head trees)))
  | otherwise = 0
