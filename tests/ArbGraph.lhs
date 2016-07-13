> module ArbGraph
>   ( Node(..), Graph, Matching, ArbGraph(..)
>   , arbGraph
>   ) where

> import Test.QuickCheck

Modules to provide data structures

> import Data.List ( foldl', intersperse, sort )

 > import Control.Monad ( replicateM )

> import qualified Data.Set as S
> import qualified Data.Map as M


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
> arbitraryGraph size
>   = do m <- floor . sqrt . (fromIntegral :: Int -> Double) <$> choose (1, size ^ (2 :: Int))
>        l <- choose (1, m)
>        r <- choose (ceiling $ (fromIntegral m :: Double) / fromIntegral l, 2 * m - l)
>        lds <- map succ <$> arbitrarySum l (m - l)
>        let n = l + r
>            ls = [N 1 :: Node Left .. N l]
>            rs = [N (l+1) :: Node Right .. N n]
>        ArbGraph . S.fromList . concat
>          <$>
>          sequence
>          (zipWith (\l d -> map ((,) l) . take d <$> shuffle rs) ls lds)


 `base l n` generates a minimal set of edges that guarantees exactly
 `n` nodes, `l` of which being left.

 > base :: Int -> Int -> Gen (S.Set (Node Left, Node Right))
 > base l n
 >   = do

          Find Right partners in range (l+1,n) for all Nodes Left.

 >        lrs <- connect (,) (l+1,n) [N 1 :: Node Left .. N l]

          For all still unpaired Nodes Right, find partners in the
          range (1,l).

 >        let unpaired
 >              = S.toList $ S.difference
 >                (S.fromAscList [N (l+1) :: Node Right .. N n])
 >                (S.fromList $ map snd lrs)
 >        rls <- connect (flip (,)) (1,l) unpaired

          Return combined edge set

 >        return $ S.union (S.fromList lrs) (S.fromList rls)


 `connect edge range nodes` is used to randomly connect all `nodes`
 with one chosen from the `range`.  It is used in both directions, so
 to accommodate the typed endpoints of the edges, the `edge`
 constructor is passed as argument, once `(,)`, and once `flip (,)`.

 > connect :: (Node a -> Node b -> c)
 >         -> (Int,Int) -> [Node a]
 >         -> Gen [c]
 > connect edge (lo,hi) = foldl' f (return [])
 >   where
 >     f acc l = (\i ps -> edge l (N i) : ps) <$> choose (lo,hi) <*> acc

     *Main> sample $ connect (,) (5,10) [N 1 .. N 4]





 Randomly choose a Node with label in given range.

 > randomNode :: (Int, Int) -> Gen (Node t)
 > randomNode = fmap N . choose


 Randomly choose an Edge with the left Node in (1,l), and the right one
 in (l+1,l+r).

 > randomEdge :: Int -> Int -> Gen (Node Left, Node Right)
 > randomEdge l r = (,) <$> randomNode (1,l) <*> randomNode (l+1, l+r)


----------------------------------------------------------------------
Generally useful generators


Chose `n` random integer values in the range `(0,s)` so that their sum
is s.  Idea: choose `k=n-1` in the range `(0,s)`, sort them `r0 < … <
rk`, then use the differences `[r0-0, r1-r0, r2-r1, ..., max-rk]`

> arbitrarySum :: Int -> Int -> Gen [Int]
> arbitrarySum n s
>   = do xs <- sequence . replicate (n-1) $ choose (0,s)
>        return . diffs $ sort xs
>   where
>     diffs xs = zipWith (-) (xs ++ [s]) (0 : xs)

    sample $ (\xs -> (length xs, sum xs, xs)) <$> arbitrarySum 4 100


----------------------------------------------------------------------
Getting test data


`arbGraph n` returns a random bipartite graph with n+2 nodes.

> arbGraph :: Int -> IO Graph
> arbGraph n = graph <$> generate (resize n arbitrary)


======================================================================
