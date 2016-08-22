
Command line program to calculate the size of a MCBM using FGL's maxFlow
algorithm.  Used for performance testing.

> module Main ( main ) where

> import FglInterface as F
> import System.Environment ( getArgs )


> from :: FilePath -> IO String
> from "-" = getContents
> from fn = readFile fn

> main :: IO ()
> main
>     = do as <- getArgs
>          case as of
>            [i] -> print . F.matchingSize . F.graph . pairs . lines =<< from i
>            other -> putStrLn  "This minimalist program reads a graph from the file given, and prints\nthe size of a maximum flow a calculated with FGL after adding source\nand sink."
>     where
>     pairs (l:ls) = case words l of
>                      (x:y:_) -> (read y :: Int, read x :: Int) : pairs ls
>                      _ -> pairs ls
>     pairs [] = []
