main :: IO ()
main = do
        contents <- readFile "Day2_input.txt"
        print $ p1 (lines contents)
        print $ p2 (lines contents)


p1 lines = length [l | l <- map check lines, l]

p2 lines = length [l | l <- map check2 lines, l]

parse :: String -> (Int, Int, Char, String)
parse line =
  let (rule, pwd) = split ':' line
      (p1, letter) = split ' ' rule
      (min, max) = split '-' p1
  in (read min, read max, letter !! 0, pwd)


check line =
  let (min, max, letter, pwd) = parse line
      count = cnt letter pwd
      valid = count >= min && count <= max
  in valid

check2 line =
  let (low, high, letter, pwd) = parse line
      valid = (pwd !! low == letter) /= (pwd !! high == letter)
  in valid


split = split_ ""

split_ :: String -> Char -> String -> (String, String)
split_ acc _ [] = (acc,"")
split_ acc splitAt (c:str)
  | splitAt == c = (acc, str)
  | otherwise = split_ (acc ++ [c]) splitAt str


cnt c xs = length [1 | x <- xs, x == c]
