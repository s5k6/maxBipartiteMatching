> {-|
> Description : Maximum cardinality bipartite matching
> Copyright   : © 2012 Stefan Klinger <http://stefan-klinger.de/>
> License     : GNU AGPL 3 <http://www.gnu.org/licenses/agpl-3.0.html>
> Maintainer  : http://stefan-klinger.de
> Stability   : experimental
> 
> Find a maximum cardinality matching on a bipartite graph, using an
> augmenting path algorithm.  More efficient than using MaxFlow from FGL
> with constant weight and additional source and sink nodes.  But not as
> good as Hopcroft–Karp would be.
> -}

> module Data.Graph.MaxBipartiteMatching ( matching ) where

> import qualified Data.Map.Strict as M
> import qualified Data.Set as S
> import Data.List ( foldl' )


Basics
------

A bipartite graph has “left” and “right” nodes, we assume the types α
and β for them.  Each edge then is an (α,β) pair, i.e., there are no
edges between nodes on the same side.

A “matching” is a subset of the edges, so that each node is incident to
at most one edge that is in the matching.  We are looking for a matching
that contains as many edges as possible.  With respect to a fixed
matching, an edge is called “matched” iff it is part of the matching.  A
node is called “free” iff it is not incident to any matched edge.

An “augmenting path” contains no cycles, starts at a free α-node,
terminates at a free β-node, and strictly alternately traverses
unmatched and matched edges.  Thus, exactly the first and last node of
an augmenting path are free, the inner nodes are not.

The algo is based on the idea of repeatedly finding an augmenting path
with respect to a current matching, starting from the empty matching,
similar to Hopcroft–Karp.  When an augmenting path is found, all of its
matched edges become unmatched, and vice versa, thus incrementing the
matching's size by one.


Implementation
--------------

The input graph is of type `Set (α,β)` which implies being bipartite and
simple.  It also denies isolated nodes, but they cannot be matched
anyways.

When looking for an augmenting path, travelling “right” to a β-node, is
always via an unmatched edge, and travelling “left” to an α-node is
always via a matched edge.  Since a β-node can have at most one matched
edge, it is sufficient to store the matching in a map of type `Map β α`,
i.e., backwards.  The invariant is being a proper matching, i.e., being
injective.

> {-|
> Return a maximum cardinality matching.  The input graph is a 'S.Set'
> (α,β) of edges, which implies being bipartite and simple.  The matching
> is returned as an injective 'M.Map' β α, i.e., backwards.
> 
> >>> matching $ Data.Set.fromList [(1,'a'),(1,'b'),(2,'a'),(2,'c'),(3,'b')]
> fromList [('a',1),('b',3),('c',2)]
> 
> -}

> matching :: (Ord a, Ord b) => S.Set (a,b) -> M.Map b a
> matching g = opt (M.keys fwd, []) fwd M.empty
>     where

Travelling right, we can choose any unmatched edge.  To this end, the
entire graph is maintained as a “forward mapping” of type `Map α [β]`,
listing all β-nodes adjacent to an α-node.

>     fwd = foldl' (\m (x,y) -> M.insertWith (++) x [y] m) M.empty $ S.toList g

Given two lists of (initially all) free and (initially no) failed
α-nodes, the forward mapping, and an (initially empty) matching, the
optimizer function…

> opt :: (Ord a, Ord b) => ([a],[a]) -> M.Map a [b] -> M.Map b a -> M.Map b a

…repeatedly calls `right` on each free α-node, i.e., starts a path
search from `x`, hoping to get a better matching back.

If no better matching is found, then `x` is set aside as a failed node
for reconsideration in later iterations.  Otherwise, `x` is part of the
matching and removed from the list of free nodes.  Also, the failed
nodes set aside previously are appended to the free nodes, since they
may lead to an augmenting path with the new matching.

> opt (x:free,failed) fwd mat
>   = either (flip (opt (free,x:failed)) mat) (opt (free++failed,[]) fwd)
>     $ right fwd [] x
>   where

`right` returns either `Right` a better matching if an augmenting path
starting at `x` was found, or `Left` a reduced forward mapping
otherwise.  The rationale is, that if no augmenting path was found, then
all α-nodes traversed during this run of `opt` can be omitted from
further searches until the matching is modified: They cannot become part
of any augmenting path.

Note, that `opt` always applies `right` to a free node, and the complete
original forward mapping, while `left` always applies `right` to a
non-free node and a reduced forward mapping.  The argument `path`
accumulates the augmenting path.

The remaining forward mapping `rem` yields a list of β-nodes that can be
reached from `x`.  We try to walk left from these, without ever going
back to `x`, which is why it's deleted from `rem`.

>     right rem path x
>       = maybe (Left rem) (left $ M.delete x rem) $ M.lookup x rem
>       where
>         left rem [] = Left rem

For the first reacheble β-node `y`, we check in line (C) below wheter
it is free, i.e., *not* in the matching `mat`.

If so, then `path'` is used to augment the matching.  We need to augment
anyways, so we can do this here and return a new matching instead of
returning a path and augment later.  Also, we don't collect multiple
paths before augmentation, because that would require to protect the
still-free β-node `y` from being used in another augmenting path.  Enjoy
this happening in line (A).

However, if `y` is not free, (i.e., in the matching) we try in line
(B) to continue where the matched edge leads to, descending `right`
from there.  If that fails, we try one of the remaining `ys`,
recursing `left`.

>         left rem (y:ys)
>           = maybe
>             (Right $ foldr (uncurry $ flip M.insert) mat path') --A
>             (either (flip left ys) Right . right rem path')     --B
>             (M.lookup y mat)                                    --C


The new path is made up of the currently last edge (x,y) and what we
have seen on the DFS so far,

>           where
>             path' = (x,y):path

Finally, when no more improvements are found for any potential left
node, i.e., all free α-nodes failed, then the current matching is
returned.

> opt ([],failed) fwd mat = mat

 
================================================================================
