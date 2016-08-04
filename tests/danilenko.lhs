> module Main ( main ) where

> import DanilenkoMatcher
> import qualified Data.Set as S
> import qualified Data.Map as M
> import System.Environment ( getArgs )


> from :: FilePath -> IO String
> from "-" = getContents
> from fn = readFile fn

> edgeList :: (Ord a, Ord b) => [(a,b)] -> [(a,[b])]
> edgeList
>   = M.toList
>     .
>     M.map S.toList
>     .
>     foldl (\m (x,y) -> M.insertWith S.union x (S.singleton y) m) M.empty

> edgeCount :: [(a,[b])] -> Int
> edgeCount = sum . map (length . snd)

> main :: IO ()
> main
>     = do as <- getArgs
>          case as of
>            [i] -> print . edgeCount . maximumMatching . edgeList . pairs . lines =<< from i
>            other -> putStrLn "FIXME"
>     where
>     pairs (l:ls) = case words l of
>                      (x:y:_) -> (read x :: Int, read y :: Int) : pairs ls
>                      _ -> pairs ls
>     pairs [] = []