> module QCHelper where

> import Test.QuickCheck
> import Data.List ( sort )

----------------------------------------------------------------------
Generally useful generators


> diffs :: Num a => [a] -> [a]
> diffs xs = zipWith (flip (-)) xs $ tail xs

> undiffs :: Num a => a -> [a] -> [a]
> undiffs z [] = []
> undiffs z (x:xs) = let y = z+x in seq y $ y : undiffs y xs


Chose `n` random integer values in the range `(0,s)` so that their sum
is `s`.  Idea: choose `k=n-1` in the range `(0,s)`, sort them `r0 < …
< rk`, then use the differences `[r0-0, r1-r0, r2-r1, ..., max-rk]`

> arbitrarySum :: Int -> Int -> Gen [Int]
> arbitrarySum n s
>   = do xs <- sequence . replicate (n-1) $ choose (0,s)
>        return . diffs $ 0 : sort xs ++ [s]

    sample $ (\xs -> (length xs, sum xs, xs)) <$> arbitrarySum 4 100



Chose `n` distinct random integer values from the range `(lo, hi)`.
Idea: Choose deltas `n` from the range `(0, hi-lo-n+1)`, add one to
each, and use the sorted deltas to calculate the numbers.

> arbitraryDistinct :: Int -> (Int,Int) -> Gen [Int]
> arbitraryDistinct n (lo,hi)
>   | hi-lo-n+1 < 0
>       = error $ "arbitraryDistinct: Cannot choose "++show n
>           ++" distinct from "++show (lo,hi)
>   | otherwise
>       = do rs <- sequence . replicate n $ choose (0, hi-lo-n+1)
>            return . undiffs (lo-1) . map (+1) . diffs . (0 :) $ sort rs



    let test (lo,hi) xs = let { n = length xs;  avg = sum xs `div` n; mid = xs !! (div n 2); a = minimum xs; b= maximum xs } in (n, mid, avg, lo <= a && b <= hi, a, b)

    let foo n range = sample $  test range <$> arbitraryDistinct n range
  


