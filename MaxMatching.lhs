
> module MaxMatching ( maxMatching ) where


Find a maximum cardinality matching on a bipartite graph [1].  Closely
modelled after the Hopcroft–Karp algorithm [2].


> import qualified Data.Map as M
> import qualified Data.Set as S
> import Control.Monad ( msum )


The input graph is of type `Set (α,β)` which implies being bipartite
and simple.  Unfortunately, it also denies isolated nodes, but they
cannot be matched anyways.

An edge is called “matched” iff it is part of a matching.  A node is
called “free” iff it is not incident to any matched edge.  We say the
α-nodes to be on the “left”, the β-nodes on the “right”.

The algo is based on the idea of repeatedly finding and applying an
augmenting path with respect to a current matching, starting from the
empty matching.  Similar to Hopcroft–Karp [2].

An augmenting path contains no cycles, starts at a free α-node,
terminates at a free β-node, and strictly alternately traverses
unmatched and matched edges.  Thus, travelling “right” to a β-node, is
always via an unmatched edge, and travelling “left” to an α-node is
always via a matched edge.  Exactly the first and last node of an
augmenting path are free, the inner nodes are not.  Since a β-node can
have at most one matched edge, it is sufficient to store the matching
in a map of type `Map β α`, i.e., backwards, with the additional
invariant of being injective.

> maxMatching :: (Ord a, Ord b) => S.Set (a,b) -> M.Map b a
> maxMatching g = optimise (targets g) (S.map fst g) M.empty


Travelling right may offer multiple choices since we can choose any
unmatched edge.  To this end the graph is maintained as a `M.Map α
[β]`, listing all β-nodes reachable from an α-node.

To avoid running in cycles, inside each run of `findPath`, the visited
inner α-nodes are removed from this mapping.  This is sufficient,
since travelling left is always via a matched edge, and thus cannot
reach the free startnode.

> targets :: (Ord a, Ord b) => S.Set (a,b) -> M.Map a [b]
> targets = foldr (\(x,y) -> M.insertWith (++) x [y]) M.empty . S.toList


Given the targets of the graph, and a current matching, `findPath tgts
mat x` tries to find an augmenting path starting from α-node `x`.
Note, that it is initially applied on a free α-node `x`, but in the
recursion it will be used on unfree nodes only, found through the
backwards mapping of the current matching.

> findPath :: (Ord a, Ord b) => M.Map a [b] -> M.Map b a -> a -> Maybe [(a,b)]
> findPath tgts mat x = M.lookup x tgts >>= msum . map back
>     where

`back y` looks for a matched edge from β-node `y`.  If there is none,
then `y` is unmatched, and (x,y) constitutes the last edge of an
augmenting path.  If there is (z,y), then we recurse trying to find
an augmenting path from `z`.  If that succeeds, we add (x,y) to it,
otherwise return Nothing.  For the recursion, `x` is removed from the
targets (it is already part of the augmenting path), so that above
`M.lookup` fails later on, canceling a loop.

>     -- back :: b -> Maybe [(a,b)] -- lookup ‘Scoped Type Variables’
>     back y = fmap ((x,y):)
>              . maybe (Just []) (findPath (M.delete x tgts) mat)
>              $ M.lookup y mat


Given the targets, a set of free α-nodes, and an initial matching,
`optimise tgts free mat` repeatedly improves the matching by finding
and applying augmenting paths, until no more paths can be found.
`mat'` is the matching with the augmenting path applied, and `free'`
is the set of free nodes left after applying the path.

OPTIMISATION: It would is sufficient to remove the newly added nodes,
i.e., the first and last node of the path!  The `targets` mapping left
over from `findPath` minus the startnode might be sufficient?

> optimise :: (Ord a, Ord b) => M.Map a [b] -> S.Set a -> M.Map b a -> M.Map b a
> optimise tgts free mat
>     = maybe mat recurse . msum . map (findPath tgts mat) $ S.toList free
>     where
>     recurse p = optimise tgts free' mat'
>         where
>         free' = foldl (flip S.delete) free $ map fst p
>         mat' = foldl (flip . uncurry $ flip M.insert) mat p

          
____________________
[1] http://en.wikipedia.org/wiki/Maximum_matching#Maximum_matchings_in_bipartite_graphs
[2] http://en.wikipedia.org/wiki/Hopcroft-Karp_algorithm
