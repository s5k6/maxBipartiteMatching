
> module FglMatcher where
>
> import qualified Data.Graph.Inductive.Graph as G
> import qualified Data.Graph.Inductive.PatriciaTree as G
> import qualified Data.Graph.Inductive.Query.MaxFlow as G
> import qualified Data.Set as S


> convGraph :: S.Set (Int, Int) -> G.Gr () Int
> convGraph g
>     = let es = S.toList g
>           ls = S.toList . S.fromList $ map fst es
>           rs = S.toList . S.fromList $ map snd es
>           enter = map (\x -> (-1, x)) ls
>           leave = map (\x -> (x, -2)) rs
>       in mkGraph (-1 : -2 : ls ++ rs) (enter ++ leave ++ es)

> mkGraph :: [Int] -> [(Int, Int)] -> G.Gr () Int
> mkGraph ns es
>   = G.mkGraph
>     (map (\x -> (x, ())) ns)         -- nodes labeled with ()
>     (map (\(a,b) -> (a, b, 1)) es)   -- edges labeled with capacity 1

> fglMatchingSize :: S.Set (Int, Int) -> Int
> fglMatchingSize g = G.maxFlow (convGraph g) (-1) (-2)
