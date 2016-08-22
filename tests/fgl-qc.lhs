
QuickCheck test compares MCBM size calculated using my and FGL's
algorithm.

> {-# LANGUAGE TemplateHaskell #-}

> import ArbGraph
> import FglInterface as F

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
>   = M.size (matching g)
>     ==
>     (F.matchingSize $ F.graph $ S.toList g)


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
