> {-# LANGUAGE TemplateHaskell #-}

> import ArbGraph
> import qualified DanilenkoInterface as D

> import Data.Graph.MaxBipartiteMatching ( matching )
> import System.Environment ( getArgs )
> import System.Exit ( exitFailure )
> import Test.QuickCheck
> import qualified Data.Map as M
> import qualified Data.Set as S


----------------------------------------------------------------------
Helper functions for properties


> swap :: (a, b) -> (b, a)
> swap (x, y) = (y, x)



Get the graph induced by the matching.

> isSubgraphOf :: D.Graph -> D.Graph -> Bool
> isSubgraphOf a b
>   = M.isSubmapOfBy S.isSubsetOf (conv a) (conv b)
>   where
>     conv = M.map S.fromAscList . M.fromAscList


----------------------------------------------------------------------
Properties


A matching calculated with Danilenko's implementation will have the
same size.

> prop_samesize :: ArbGraph -> Bool
> prop_samesize (ArbGraph g)
>   = M.size (matching g)
>     ==
>     (D.edgeCount . D.matching . D.graph $ S.toList g)



The graph induced by a matching must be a subgraph of the original.

> prop_subset :: ArbGraph -> Bool
> prop_subset (ArbGraph g)
>   = (D.matching . D.graph $ S.toList g)
>     `isSubgraphOf`
>     (D.graph $ S.toList g)



The graph with reversed edges produces a matching of the same size.

> prop_symmetric :: ArbGraph -> Bool
> prop_symmetric (ArbGraph g)
>   = (D.edgeCount . D.matching . D.graph $ S.toList g)
>     ==
>     (D.edgeCount . D.matching . D.graph . map swap $ S.toList g)


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
