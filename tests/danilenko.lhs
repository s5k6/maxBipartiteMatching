
Command line program to calculate the size of a MCBM using Danilenko's
algorithm.  Used for performance testing.

> module Main ( main ) where

> import DanilenkoInterface
> import System.Environment ( getArgs )


> from :: FilePath -> IO String
> from "-" = getContents
> from fn = readFile fn

> main :: IO ()
> main
>     = do as <- getArgs
>          case as of
>            [i] -> print . edgeCount . matching . graph . pairs . lines =<< from i
>            other -> putStrLn "This minimalist program reads a graph from the file given, and prints\nthe size of a MCBM calculated with Danilenko's implementation."
>     where
>     pairs (l:ls) = case words l of
>                      (x:y:_) -> (read x :: Int, read y :: Int) : pairs ls
>                      _ -> pairs ls
>     pairs [] = []
