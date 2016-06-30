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

> import Data.List ( foldl', sort )
> import Control.Monad ( replicateM )
> import qualified Data.Set as S
> import qualified Data.Map as M

----------------------------------------------------------------------

Use new type for nodes to control QuickCheck's generation of test
cases.  Use a phantom type to distinguish left and right nodes.

> newtype Node a = N Int deriving (Eq, Ord, Show)
> data Left
> data Right

 > instance Arbitrary (Node a) where

     The following would only use the generation of Integers.

  >   arbitrary = N <$> arbitrary

 >   arbitrary = sized $ \s -> N <$> oneof (map return [1..s])


Types to represent graphs with such nodes, and matchings calculated
for them.

> newtype Graph = G { edges :: S.Set (Node Left, Node Right) }
> instance Show Graph where show = show . edges
> type Matching = M.Map (Node Right) (Node Left)

> instance Arbitrary Graph where
>   arbitrary = sized arbitraryGraph

> arbitraryGraph :: Int -> Gen Graph
> arbitraryGraph n0
>   = do let n = n0 + 2
>        l <- choose (1, n-1)
>        let r = n - l
>            ls = map N [1..l] :: [Node Left]
>            rs = map N [l+1..n] :: [Node Right]
>            min_m = max l r
>        m <- choose (min_m, l * r)
>        G as <- foo (S.fromList ls) (S.fromList rs)
>        bs <- S.fromList <$> replicateM m (randomEdge l r)
>        return . G $ S.union as bs

> randomNode :: (Int, Int) -> Gen (Node t)
> randomNode = fmap N . choose
>
> randomEdge :: Int -> Int -> Gen (Node Left, Node Right)
> randomEdge l r = (,) <$> randomNode (1,l) <*> randomNode (l+1, l+r)

> foo :: S.Set (Node Left) -> S.Set (Node Right) -> Gen Graph
> foo ls rs = do ps <- bar (,) [] (S.toList ls) rs
>                let rs' = S.difference rs . S.fromList $ map snd ps
>                ps' <- bar (flip (,)) ps (S.toList rs') ls
>                return . G $ S.fromList ps'

> bar :: (Node a -> Node b -> c) -> [c] -> [Node a] -> S.Set (Node b) -> Gen [c]
> bar pair z ls rs = foldl' f (return z) ls
>   where
>     f acc l = (\i ps -> pair l (S.elemAt i rs) : ps) <$> choose (0, S.size rs - 1) <*> acc

Get the graph induced by the matching.

> induced :: Matching -> Graph
> induced = G . S.fromList . map (uncurry $ flip (,)) . M.toAscList


----------------------------------------------------------------------
Properties


The graph induced by a matching must be a subgraph of the original.

> prop_subset :: Graph -> Bool
> prop_subset (G g) = edges (induced $ matching g) `S.isSubsetOf` g


The matching is returned in a M.Map, which must be injective.

> prop_injective :: Graph -> Bool
> prop_injective = diff . sort . map snd . M.toList . matching . edges
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

> main :: IO ()
> main = $(quickCheckAll) >> return ()


======================================================================
