> {-# LANGUAGE TemplateHaskell #-}

> import ArbGraph
> import qualified DanilenkoMatcher as D

> import Data.Graph.MaxBipartiteMatching ( matching )
> import System.Environment ( getArgs )
> import System.Exit ( exitFailure )
> import Test.QuickCheck
> import qualified Data.Map as M
> import qualified Data.Set as S


----------------------------------------------------------------------

> edgeList :: Graph -> D.Graph
> edgeList g
>   = M.toList
>     .
>     M.map S.toList
>     .
>     foldl (\m (x,y) -> M.insertWith S.union x (S.singleton y) m) M.empty
>     $
>     [ e
>     | (x,y) <- S.toList g
>     , let x' = fromEnum x
>           y' = fromEnum y
>     , e <- [(x',y'), (y',x')]
>     ]

> edgeCount :: [(a,[b])] -> Int
> edgeCount = (`div` 2) . sum . map (length . snd)

----------------------------------------------------------------------
Properties


A matching calculated with Nikita Danilenko's implementation will have
the same size.

> prop_samesize :: ArbGraph -> Bool
> prop_samesize (ArbGraph g)
>   = disjoint g
>     &&
>     M.size (matching g)
>     ==
>     (edgeCount . D.maximumMatching $ edgeList g)



The FGL matcher does not provide different namespaces for left and
right nodes, so we need to make sure that the integer labels of the
nodes are distinct.

> disjoint :: Graph -> Bool
> disjoint g
>   = S.null $ S.intersection (S.fromList $ map fromEnum ls)
>                             (S.fromList $ map fromEnum rs)
>   where
>   (ls, rs) = unzip $ S.toList g


----------------------------------------------------------------------
Checking all properties


  * This requires the TemplateHaskell language extension, see pragma
    at very top of the file.

  * Insufficient documentation, but see

        http://stackoverflow.com/questions/5683911/
        http://stackoverflow.com/questions/8976488/

> return [] -- need this for GHC 7.8

> main :: IO ()
> main
>   = do args <- parseArgs <$> getArgs
>        s <- $(forAllProperties) $ quickCheckWithResult args
>        s ? return () $ exitFailure
>   where
>     parseArgs as
>       = null as ? stdArgs $ stdArgs{ maxSuccess = read $ head as }

> infix 1 ?
> (?) :: Bool -> t -> t -> t
> c ? x = \y-> if c then x else y

======================================================================
