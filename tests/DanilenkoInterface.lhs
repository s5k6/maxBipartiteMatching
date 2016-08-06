> module DanilenkoInterface
>        ( Graph, matching, graph, edgeCount
>        ) where
>
> import qualified Data.Set as S
> import qualified Data.Map as M
> import DanilenkoOriginal

> matching :: Graph -> Graph
> matching = maximumMatching

> graph :: (Ord a, Ord b) => [(a, b)] -> Graph
> graph es
>   = merge 0    -- merge in nodes with degree 0
>     .
>     M.toList   
>     .
>     M.map S.toList  -- list outgoing nodes
>     .
>     foldl     -- aggregate outgoing edges per node
>     (\m (x,y) -> M.insertWith S.union x (S.singleton y) m)
>     M.empty
>     $
>     [ e'
>     | (x,y) <- es
>     , let x' = left x     -- map node IDs to consecutive integers
>           y' = right y
>     , e' <- [ (x', y'), (y', x') ]  -- symmetric directed graph
>     ]
>   where

>     -- map node IDs to consecutive integers
>     ls = S.fromList $ map fst es
>     lc = S.size ls
>     left l = maybe undefined id $ S.lookupIndex l ls
>     rs = S.fromList $ map snd es
>     right r = maybe undefined (+ lc) $ S.lookupIndex r rs

>     -- paper: need all nodes, even with empty edge list!
>     merge n all@((x,ys):rest)
>       = case compare n x of
>           LT -> (n,[]) : merge (n+1) all
>           GT -> undefined
>           EQ -> (x,ys) : merge (n+1) rest
>     merge _ [] = []


Number of edges is half (bc. symmetric) of lengths of adjacency lists.

> edgeCount :: Graph -> Int
> edgeCount = flip div 2 . sum . map (length . snd)

