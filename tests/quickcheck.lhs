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

> import Data.List ( foldl', sort, intersperse )
> import Control.Monad ( replicateM )
> import qualified Data.Set as S
> import qualified Data.Map as M
> import System.Environment ( getArgs )
> import System.Exit ( exitFailure )

----------------------------------------------------------------------
Generation of test data


Use new type for nodes to control QuickCheck's generation of test
cases.  Use a phantom type to distinguish left and right nodes.

> newtype Node phantom = N Int deriving (Eq, Ord, Show)
> data Left
> data Right

> instance Enum (Node t) where
>   toEnum = N
>   fromEnum (N x) = x


Types to represent graphs with such nodes, and matchings calculated
for them.

> type Graph = S.Set (Node Left, Node Right)
> type Matching = M.Map (Node Right) (Node Left)

We need a new type `Graph` to specialize `Arbitrary` in a
way that generates a nicer variety of bipartite graphs.

> newtype ArbGraph = ArbGraph { graph :: Graph }

> instance Show ArbGraph where
>   showsPrec _ g = ss "{" . (foldl' (.) id . intersperse (ss ", ")
>                                 . map sn . S.toList $ graph g) . ss "}"
>     where
>       ss = showString
>       sc = showChar
>       sn (N a, N b) = sc '(' . shows a . sc ',' . shows b . sc ')'


Nodes must be arbitrary to allow shrinking of ArbGraph, which it is a
subterm of.

> instance Arbitrary (Node t) where arbitrary = N <$> arbitrary

> instance Arbitrary ArbGraph where
>   arbitrary = sized arbitraryGraph
>   shrink = map ArbGraph . shrink . graph

> arbitraryGraph :: Int -> Gen ArbGraph
> arbitraryGraph n0
>   = do let n = n0 + 2
>        l <- choose (1, n-1)
>        let r = n - l
>        m <- choose (max l r, l * r)

         Find Right partners in range (l+1,n) for all Nodes Left.

>        lrs <- bar (,) (l+1,n) [N 1 :: Node Left .. N l]

         For all still unpaired Nodes Right, find partners in the
         range (1,l).

>        let unpaired
>              = S.toList $ S.difference
>                (S.fromAscList [N (l+1) :: Node Right .. N n])
>                (S.fromList $ map snd lrs)
>        rls <- bar (flip (,)) (1,l) unpaired

         Generate at most m extra random edges.

>        es <- S.fromList <$> replicateM m (randomEdge l r)
>        return . ArbGraph $ S.unions [S.fromList lrs, S.fromList rls, es]

Randomly choose a Node with label in given range.

> randomNode :: (Int, Int) -> Gen (Node t)
> randomNode = fmap N . choose

Randomly choose an Edge with the left Node in (1,l), and the right one
in (l+1,l+r).

> randomEdge :: Int -> Int -> Gen (Node Left, Node Right)
> randomEdge l r = (,) <$> randomNode (1,l) <*> randomNode (l+1, l+r)


> bar :: (Node a -> Node b -> c)
>     -> (Int,Int) -> [Node a]
>     -> Gen [c]
> bar edge (lo,hi) = foldl' f (return [])
>   where
>     f acc l = (\i ps -> edge l (N i) : ps) <$> choose (lo,hi) <*> acc

  *Main> sample $ bar (,) [] (5,7) [N 1 .. N 4]


> swap :: (a, b) -> (b, a)
> swap (x, y) = (y, x)


Get the graph induced by the matching.

> induced :: Matching -> Graph
> induced = S.fromList . map swap . M.toAscList


----------------------------------------------------------------------
Properties


The graph induced by a matching must be a subgraph of the original.

> prop_subset :: ArbGraph -> Bool
> prop_subset (ArbGraph g) = induced (matching g) `S.isSubsetOf` g


The matching is returned in a M.Map, which must be injective.

> prop_injective :: ArbGraph -> Bool
> prop_injective
>   = diff . sort . tail . map snd . M.toList . matching . graph
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


A property that fails to see if tests are actually performed

> prop'_fail = const False


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
> c ? x = \y-> if c then x else y

======================================================================
