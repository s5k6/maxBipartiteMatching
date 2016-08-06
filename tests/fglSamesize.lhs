> {-# LANGUAGE TemplateHaskell #-}

> import ArbGraph
> import FglMatcher

> import Data.Graph.MaxBipartiteMatching ( matching )
> import System.Environment ( getArgs )
> import System.Exit ( exitFailure )
> import Test.QuickCheck
> import qualified Data.Map as M
> import qualified Data.Set as S
> import Helper


----------------------------------------------------------------------
Properties


A matching calculated with FGL's maxFlow will have the same size.

> prop_samesize :: ArbGraph -> Bool
> prop_samesize (ArbGraph g)
>   = disjoint g
>     &&
>     M.size (matching g)
>     ==
>     fglMatchingSize (S.map (\(a,b) -> (fromEnum a, fromEnum b)) g)



The FGL matcher does not provide different namespaces for left and
right nodes, so we need to make sure that the integer labels of the
nodes are distinct.

> disjoint :: Graph -> Bool
> disjoint g
>   = S.null $ S.intersection (S.fromList $ map fromEnum ls)
>                             (S.fromList $ map fromEnum rs)
>   where
>   (ls, rs) = unzip $ S.toList g



Producing a non-disjoint graph (in the sense tested above) is a
failure in this testsuite.

> prop_selftest :: ArbGraph -> Bool
> prop_selftest (ArbGraph g)
>   = disjoint g


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


======================================================================
