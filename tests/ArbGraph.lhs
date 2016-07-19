> module ArbGraph
>   ( Node(..), Graph, Matching, ArbGraph(..)
>   , arbGraph
>   ) where

> import Test.QuickCheck
> import QCHelper
> import Data.List ( foldl', intersperse )
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



Ok, the third try to generate sensibly distributed random bipartite
graphs.

> arbitraryGraph :: Int -> Gen ArbGraph
> arbitraryGraph size
>   = do n <- sqrt' <$> choose (sqr 2, sqr size)
>        l <- choose (1, n-1)
>        let r = n - l
>        m <- choose (max l r, l * r)
>        let rem = 2*m > l*r
>            edge k = let (a,b) = k `divMod` r
>                     in (N (a+1), N (l+b+1)) :: (Node Left , Node Right)
>        xs <- arbitraryDistinct (if rem then l * r - m else m) (0, l * r - 1)
>        return . ArbGraph . S.fromAscList . map edge
>            $
>            if rem then remove xs [0 .. l * r - 1] else xs

> sqr :: Int -> Int
> sqr x = x * x

> sqrt' :: Int -> Int
> sqrt' = floor . sqrt . (fromIntegral :: Int -> Double)

Under the precondition that both arguments are sorted in ascending
order, `remove xs ys` removes all elements in `xs` from the list `ys`.
A single occurrence in `xs` removes all occurrences in `ys`.  Multiple
occurrences in `xs` are not relevant.

> remove :: Ord a => [a] -> [a] -> [a]
> remove (x:xs) ys
>   = let (as, bs) = span (< x) ys
>     in as ++ remove xs (dropWhile (== x) bs)
> remove _ ys = ys



Better distribution, first successful version.  Very slow for large graphs.

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



Original implementation with Leo

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
Getting test data


`arbGraph n` returns a random bipartite graph with n+2 nodes.

> arbGraph :: Int -> IO Graph
> arbGraph n = graph <$> generate (resize n arbitrary)


======================================================================
