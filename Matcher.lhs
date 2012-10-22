> module Matcher ( main ) where

> import MaxMatching
> import qualified Data.Set as S
> import qualified Data.Map as M

> main
>     = interact
>       $ unlines . map (\(a,b) -> a++" "++b) . M.toList
>       . matching . S.fromList . pairs . lines
>     where
>     pairs (l:ls) = case words l of
>                      (x:y:_) -> (x,y) : pairs ls
>                      _ -> pairs ls
>     pairs [] = []

