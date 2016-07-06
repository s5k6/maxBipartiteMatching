> module Main ( main ) where

> import FglMatcher
> import qualified Data.Set as S
> import System.Environment ( getArgs )


> from :: FilePath -> IO String
> from "-" = getContents
> from fn = readFile fn

> main :: IO ()
> main
>     = do as <- getArgs
>          case as of
>            [i] -> putStrLn . show . fglMatchingSize . S.fromList . pairs . lines =<< from i
>            other -> putStrLn "FIXME"
>     where
>     pairs (l:ls) = case words l of
>                      (x:y:_) -> (read y :: Int, read x :: Int) : pairs ls
>                      _ -> pairs ls
>     pairs [] = []
