> module Matcher ( main ) where

> import MaxMatching
> import qualified Data.Set as S
> import qualified Data.Map as M
  
> main
>     = interact $ foldr ($) "" . map see . M.toList . matching . S.fromList . pairs . lines
>     where
>     see (a,b) = showString b . showChar ' ' . showString a . showChar '\n'
>     pairs (l:ls) = case words l of
>                      (x:y:_) -> (x,y) : pairs ls
>                      _ -> pairs ls
>     pairs [] = []
