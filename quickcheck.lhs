> {-# LANGUAGE TemplateHaskell #-}

One time setup:

    $ cabal sandbox init
    $ cabal install quickcheck
    $ cabal install maxBipartiteMatching.cabal

Running:

    $ cabal exec runhaskell quickcheck.lhs
    $ cabal exec ghci quickcheck.lhs

Reading:

  * https://wiki.haskell.org/Introduction_to_QuickCheck1


----------------------------------------------------------------------
Import


Import the QuickCheck framework.

> import Test.QuickCheck
> import Test.QuickCheck.All

The Modules we want to test

> import Data.Graph.MaxBipartiteMatching

Modules to provide data structures

> import Data.List ( foldl1', sort )
> import qualified Data.Set as S
> import qualified Data.Map as M

----------------------------------------------------------------------

Use new type for nodes to control QuickCheck's generation of test
cases.  Use a phantom type to distinguish left and right nodes.

> newtype Node a = N Int deriving (Eq, Ord, Show)
> data Left = L__index_type
> data Right = R__index_type

> instance Arbitrary (Node a) where

    The following would only use the generation of Integers.

 >   arbitrary = N <$> arbitrary

>   arbitrary = N <$> oneof (map return [1..100])


Types to represent graphs with such nodes, and matchings calculated
for them.

> type Graph = S.Set (Node Left, Node Right)
> type Matching = M.Map (Node Right) (Node Left)


Get the graph induced by the matching.

> edges :: Matching -> Graph
> edges = S.fromList . map (uncurry $ flip (,)) . M.toAscList


----------------------------------------------------------------------
Properties


The graph induced by a matching must be a subgraph of the original.

> prop_subset :: Graph -> Bool
> prop_subset g = edges (matching g) `S.isSubsetOf` g


The matching is returned in a M.Map, which must be injective.

> prop_injective :: Graph -> Bool
> prop_injective = diff . sort . map snd . M.toList . matching
>   where
>     diff [] = True
>     diff xs = and $ zipWith (/=) xs (tail xs)


----------------------------------------------------------------------
Checking all properties


  * This requires the TemplateHaskell language extension, see pragme
    at very top of the file.

  * Insufficient documentation, but see
    http://stackoverflow.com/questions/5683911/simple-haskell-unit-testing


> return [] -- need this for GHC 7.8

> main = $(quickCheckAll)


======================================================================
