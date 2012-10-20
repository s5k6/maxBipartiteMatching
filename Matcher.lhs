> module Matcher ( main ) where

> import MaxMatching
> import qualified Data.Set as S
> import qualified Data.Map as M
> import System.Environment ( getArgs )
> import Control.Applicative

> main
>     = do as <- getArgs
>          let (i,o) = case as of
>                      [] -> (getContents, putStr . snd)
>                      [f] -> (readFile f, print . fst)
>                      [f,"-"] -> (readFile f, putStr . snd)
>                      [f1,f2] -> ( readFile f1
>                                 , \(s,m) -> print s >> writeFile f2 m
>                                 )
>          g <- S.fromList . pairs . lines <$> i
>          let m = matching g
>          o (M.size m, unlines . map see $ M.toList m)
>     where
>     see (a,b) = a++" "++b
>     pairs (l:ls) = case words l of
>                      (x:y:_) -> (x,y) : pairs ls
>                      _ -> pairs ls
>     pairs [] = []
