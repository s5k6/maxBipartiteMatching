> {-# LANGUAGE TemplateHaskell #-}

One time setup:

    $ cabal sandbox init
    $ cabal install quickcheck
    $ cabal install maxBipartiteMatching.cabal

Running:

    $ cabal exec runhaskell quickcheck.lhs
    $ cabal exec ghci quickcheck.lhs

Reading:

  * https://wiki.haskell.org/Introduction_to_QuickCheck2


----------------------------------------------------------------------
Import


Import the QuickCheck framework.

> import Test.QuickCheck

The Modules we want to test

> import Data.Graph.MaxBipartiteMatching

Modules to provide data structures

> import Data.List ( foldl', sort, intersperse )
> import Control.Monad ( replicateM )
> import qualified Data.Set as S
> import qualified Data.Map as M

----------------------------------------------------------------------

Use new type for nodes to control QuickCheck's generation of test
cases.  Use a phantom type to distinguish left and right nodes.

> newtype Node phantom = N Int deriving (Eq, Ord, Show)
> data Left
> data Right

> instance Enum (Node t) where
>   toEnum = N
>   fromEnum (N x) = x

Types to represent graphs with such nodes, and matchings calculated
for them.  We need a new type `Graph` to specialize `Arbitrary` in a
way that generates nice bipartite Graphs.

> type EdgeSet = S.Set (Node Left, Node Right)
> newtype Graph = G { edges :: EdgeSet } deriving Show
> type Matching = M.Map (Node Right) (Node Left)

> instance Arbitrary (Node t) where
>   arbitrary = N <$> arbitrary

> instance Arbitrary Graph where
>   arbitrary = sized arbitraryGraph
>   shrink = map G . shrink . edges

> arbitraryGraph :: Int -> Gen Graph
> arbitraryGraph n0
>   = do let n = n0 + 2
>        l <- choose (1, n-1)
>        let r = n - l
>            ls = S.fromAscList [N 1 :: Node Left .. N l]
>            rs = S.fromAscList [N (l+1) :: Node Right .. N n]
>        m <- choose (max l r, l * r)
>        ps <- bar (,) [] rs $ S.toList ls
>        let rs' = S.difference rs . S.fromList $ map snd ps
>        as <- S.fromList <$> bar (flip (,)) ps  ls (S.toList rs')
>        bs <- S.fromList <$> replicateM m (randomEdge l r)
>        return . G $ S.union as bs

> randomNode :: (Int, Int) -> Gen (Node t)
> randomNode = fmap N . choose
>
> randomEdge :: Int -> Int -> Gen (Node Left, Node Right)
> randomEdge l r = (,) <$> randomNode (1,l) <*> randomNode (l+1, l+r)

> bar :: (Node a -> Node b -> c)
>     -> [c] -> S.Set (Node b) -> [Node a]
>     -> Gen [c]
> bar edge z rs = foldl' f (return z)
>   where
>     f acc l = (\i ps -> edge l (S.elemAt i rs) : ps)
>               <$>
>               choose (0, S.size rs - 1)
>               <*>
>               acc


> swap :: (a, b) -> (b, a)
> swap (x, y) = (y, x)


Get the graph induced by the matching.

> induced :: Matching -> Graph
> induced = G . S.fromList . map swap . M.toAscList


----------------------------------------------------------------------
Properties


The graph induced by a matching must be a subgraph of the original.

> prop_subset :: Graph -> Bool
> prop_subset (G g) = edges (induced $ matching g) `S.isSubsetOf` g


The matching is returned in a M.Map, which must be injective.

> prop_injective :: Graph -> Bool
> prop_injective
>   = diff . sort . tail . map snd . M.toList . matching . edges
>   where
>     diff [] = True
>     diff xs = and $ zipWith (/=) xs (tail xs)


A maximum cardinality matching must have the same size, no matter from
which side it is created.

> prop_symSize :: Graph -> Bool
> prop_symSize g
>   = M.size (matching . S.map swap $ edges g)
>     ==
>     M.size (matching $ edges g)


----------------------------------------------------------------------
Checking all properties


  * This requires the TemplateHaskell language extension, see pragme
    at very top of the file.

  * Insufficient documentation, but see
    http://stackoverflow.com/questions/5683911/simple-haskell-unit-testing


> return [] -- need this for GHC 7.8

> main :: IO ()
> main = $(quickCheckAll) >> return ()


======================================================================
