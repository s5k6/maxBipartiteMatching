
Find a maximum cardinality matching on a simple bipartite graph [1].
Modelled after the Hopcroft–Karp algorithm [2].

Author: Stefan Klinger <http://stefan-klinger.de>

License: GNU Affero General Public License, Version 3
         <http://www.gnu.org/licenses/agpl-3.0.html>

Date: Fri 2012-Oct-19 16:23:52 CEST


> module MaxMatching ( matching ) where

> import qualified Data.Map as M
> import qualified Data.Set as S

  
The input graph is of type `Set (α,β)` which implies being bipartite and
simple.  Unfortunately, it also denies isolated nodes, but they cannot
be matched anyways.

A “matching” is a subset of the edges, so that each node is incident to
at most one edge that is in the matching.  We are looking for a matching
that contains as many edges as possible.  Wrt. a fixed matching, an
edge is called “matched” iff it is part of the matching.  A node is
called “free” iff it is not incident to any matched edge.  We say the
α-nodes to be on the “left”, the β-nodes on the “right”.

An “augmenting path” contains no cycles, starts at a free α-node,
terminates at a free β-node, and strictly alternately traverses
unmatched and matched edges.

The algo is based on the idea of repeatedly finding an augmenting path
with respect to a current matching, starting from the empty matching,
similar to Hopcroft–Karp [2].  When an augmenting path is found, all of
its matched edges become unmatched, and vice versa, thus incrementing
the matching's size by one.


When looking for an augmenting path, travelling “right” to a β-node, is
always via an unmatched edge, and travelling “left” to an α-node is
always via a matched edge.  Exactly the first and last node of an
augmenting path are free, the inner nodes are not.  Since a β-node can
have at most one matched edge, it is sufficient to store the matching in
a map of type `Map β α`, i.e., backwards.  The invariant is being a
proper matching, i.e., being injective.

> matching :: (Ord a, Ord b) => S.Set (a,b) -> M.Map b a
> matching g = optimise False (M.keys fwd,[]) fwd M.empty
>     where
>     fwd = fwdEdges g

      
Travelling right may offer multiple choices since we can choose any
unmatched edge.  To this end the graph is maintained as a `M.Map α [β]`,
listing all β-nodes reachable from an α-node.

> fwdEdges :: (Ord a, Ord b) => S.Set (a,b) -> M.Map a [b]
> fwdEdges = foldr (\(x,y) -> M.insertWith (++) x [y]) M.empty . S.toList


Given the forward edges, an (initially empty) matching, and a zipper [3]
of free nodes, …

> optimise :: (Ord a, Ord b) => Bool -> ([a],[a]) -> M.Map a [b] -> M.Map b a
>          -> M.Map b a

… `optimise` repeatedly calls `find` on each free α-node `x`, i.e., runs
a path search from `x`, hoping to get Just a better matching back.  In
that case, `x` is removed from the zipper, otherwise it is kept for
later iterations.

> optimise more (x:xs,ys) fwd mat
>     = maybe (optimise more (xs,x:ys) fwd mat) (optimise True (xs,ys) fwd)
>       $ find fwd mat x

When no `more` improvements are found for any free node, the current
matching is returned.  Otherwise, we retry all remaining free nodes
`ys`, since the refined matching might make new augmenting paths
possible.

> optimise more ([],ys) fwd mat
>     = if more then optimise False (ys,[]) fwd mat else mat


Now the core of the implementation is `find`.  With the forward edges
and a start node `x`, it tries to improve a given matching by looking
for an augmenting path starting from `x`, which is free.  This is done
by a depth first search, alternately travelling `right` and `left.

> find :: (Ord a, Ord b) => M.Map a [b] -> M.Map b a -> a -> Maybe (M.Map b a)
> find fwd mat x = fst $ right fwd [] x
>     where

The performance boost comes from removing all visited α-nodes `x` from
the forward mapping, and carrying this information through the
backtracking.  Hence, `right` returns Maybe a better matching, and
always the `rem`aining forward mapping.  Here we bail out if `x` is not
in the forward mapping any more, avoiding cycles, and nodes that cannot
lead to an augmenting path currently.  Note, that `find` is always
called with the complete forward mapping by `optimise`.

>     right rem path x
>         = maybe (Nothing, rem) (left $ M.delete x rem) $ M.lookup x rem
>         where

The remaining forward mapping `rem` yields a list of β-nodes that can be
reached from `x`.  If that list runs empty, then there is no augmenting
path via `x`, and also not via any of the nodes we have tried below, so
we return a hopefully much smaller forward mapping `rem`.

>         left rem [] = (Nothing, rem)

For the first reacheble β-node `y`, we check wheter it is free, i.e.,
not in the matching `mat`.  If so, the `path'`, made up of the currently
last edge (x,y) and what we have seen on the DFS so far, is used to
augment the matching.  We need to augment anyways, so we can do this
here and return a matching, instead of returning a path and augment
later.  Also, we don't collect multiple paths before augmentation,
because that would require to protect the still-free β-node `y` from
being used in another augmenting path.  However, if `y` is not free, we
try to continue where the matched edge leads to, descending `right`.  If
that fails, we try one of the remaining `ys`, recursing `left`.

>         left rem (y:ys)
>             = maybe
>               (Just $ foldr (uncurry $ flip M.insert) mat path', rem)
>               (uncurry (maybe (flip left ys) ((,) . Just)) . right rem path')
>               $ M.lookup y mat
>             where
>             path' = (x,y):path

Enjoy the augmenting step: For each edge (u,v) in the `path'`, it
adjusts the backward mapping of `v` to `u` by a simple `M.insert`.

____________________
[1] https://en.wikipedia.org/wiki/Maximum_matching#Maximum_matchings_in_bipartite_graphs
[2] https://en.wikipedia.org/wiki/Hopcroft-Karp_algorithm
[3] https://en.wikipedia.org/wiki/Zipper_(data_structure)#Example:_Bidirectional_list_traversal
