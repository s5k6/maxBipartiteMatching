
Interface to FGL algorithm calculating the size of an MCBM.

> module FglInterface
>        ( graph
>        , matchingSize
>        ) where
>
> import qualified Data.Graph.Inductive.Graph as G
> import qualified Data.Graph.Inductive.PatriciaTree as G
> import qualified Data.Graph.Inductive.Query.MaxFlow as G
> import qualified Data.Set as S


> graph ::  (Ord a, Ord b) => [(a, b)] -> G.Gr () Int
> graph es
>   = annotate
>     (S.toList (S.fromList [ n | (x,y) <- es', n <- [x,y] ]))
>     es'
>   where

>     -- map node IDs to consecutive integers
>     ls = S.fromList $ map fst es
>     lc = S.size ls
>     left l = maybe undefined id $ S.lookupIndex l ls
>     rs = S.fromList $ map snd es
>     rc = S.size rs
>     right r = maybe undefined (+ lc) $ S.lookupIndex r rs
>     es' = concat
>           [ [ (left x, right y) | (x, y) <- es ]
>           , [ (-1, n) | n <- [0 .. lc - 1] ]
>           , [ (n, -2) | n <- [lc .. lc + rc - 1] ]
>           ]

> annotate :: [Int] -> [(Int, Int)] -> G.Gr () Int
> annotate ns es
>   = G.mkGraph
>     (map (\x -> (x, ())) ns)         -- nodes labeled with ()
>     (map (\(a,b) -> (a, b, 1)) es)   -- edges labeled with capacity 1

> matchingSize :: G.Gr () Int -> Int
> matchingSize g = G.maxFlow g (-1) (-2)
