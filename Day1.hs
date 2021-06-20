sum2020 xs = head [(x, y, x * y) | x <- xs, y <- xs, x + y == 2020]

sum2020x3 xs = head [(x, y, z, x * y * z) | x <- xs, y <- xs, z <- xs, x + y + z== 2020]

test :: IO ()
test =
  do
    print (sum2020 test_in)


main :: IO ()
main = do
        contents <- readFile "d1_input.txt"
        print . sum2020 . map read . words $ contents
        print . sum2020x3 . map read . words $ contents

test_in = [5, 1721,979,366,299, 675,1456]

