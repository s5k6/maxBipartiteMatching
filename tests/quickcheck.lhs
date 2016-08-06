|
  Description : Testsuite for Data.Graph.MaxBipartiteMatching
  Copyright   : © 2016 Stefan Klinger <http://stefan-klinger.de/>
  License     : GNU AGPL 3 <http://www.gnu.org/licenses/agpl-3.0.html>
  Maintainer  : http://stefan-klinger.de
  Stability   : experimental

One time setup:

    $ cabal sandbox init
    $ cabal install --enable-tests --only-dependencies

Testing:

    $ cabal test

    or

    $ dist/build/test-MaxBipartiteMatching/test-MaxBipartiteMatching 1000

    or

    $ cabal exec ghci tests/quickcheck.lhs

Reading:

  * https://wiki.haskell.org/Introduction_to_QuickCheck2


----------------------------------------------------------------------
Environment


QuickCheck uses TemplateHaskell to scan for `prop_*` functions.

> {-# LANGUAGE TemplateHaskell #-}


Import the QuickCheck framework.

> import Test.QuickCheck

The Modules we want to test

> import Data.Graph.MaxBipartiteMatching ( matching )

Modules to provide data structures

> import ArbGraph
> import Data.List ( sort )
> import qualified Data.Set as S
> import qualified Data.Map as M
> import System.Environment ( getArgs )
> import System.Exit ( exitFailure )
> import Helper

----------------------------------------------------------------------
Helper functions for properties


> swap :: (a, b) -> (b, a)
> swap (x, y) = (y, x)



Get the graph induced by the matching.

> induced :: Matching -> Graph
> induced = S.fromList . map swap . M.toList


----------------------------------------------------------------------
Properties


The graph induced by a matching must be a subgraph of the original.

> prop_subset :: ArbGraph -> Bool
> prop_subset (ArbGraph g) = induced (matching g) `S.isSubsetOf` g



The matching is returned in a M.Map, which must be injective.

> prop_injective :: ArbGraph -> Bool
> prop_injective
>   = diff . sort . map snd . M.toList . matching . graph
>   where
>     diff [] = True
>     diff xs = and $ zipWith (/=) xs (tail xs)



A maximum cardinality matching must have the same size, no matter from
which side it is created.

> prop_symSize :: ArbGraph -> Bool
> prop_symSize (ArbGraph g)
>   = M.size (matching $ S.map swap g)
>     ==
>     M.size (matching g)


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
