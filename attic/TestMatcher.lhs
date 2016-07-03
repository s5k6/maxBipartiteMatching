> module TestMatcher ( main ) where

> import Data.Graph.MaxBipartiteMatching
> import qualified Data.Set as S
> import qualified Data.Map as M
> import Control.Applicative
> import Control.Monad
> import qualified Data.Graph.Inductive.Graph as G
> import qualified Data.Graph.Inductive.PatriciaTree as G
> import qualified Data.Graph.Inductive.Query.MaxFlow as G
> import System.Environment ( getArgs, getEnv )
> import System.Random
> import Data.Array.IO
       
> assert e c = if c then return () else fail $ "assert "++e
> swap (a,b) = (b,a)
> 

> parse :: String -> S.Set (Int,Int)
> parse = S.fromList . p . lines
>     where
>     p :: [String] -> [(Int,Int)]
>     p (l:ls) = case words l of
>                (x:y:_) -> (read x, read y) : p ls
>                _ -> p ls
>     p [] = []

> rEdge :: Int -> Int -> IO (Int,Int)
> rEdge l r = (,) <$> (randomRIO (0,l)) <*> (randomRIO (0,r))

> fmt = unlines . map (\(a,b) -> show a ++ ' ':show b)

> stat g
>     = let es = S.toList g -- edge list
>           ec = length es
>           ls = S.fromList $ map fst es
>           rs = S.fromList $ map snd es
>           lc = S.size ls
>           rc = S.size rs
>       in (es,ec,ls,rs,lc,rc)


Modified `shuffle` function, taken from the Haskell Wiki:
http://www.haskell.org/haskellwiki/Random_shuffle
        
> shuffle :: Int -> [a] -> IO [a]
> shuffle k xs
>     = do ar <- newArray n xs
>          forM [1..k] $ \i -> do
>            j <- randomRIO (i,n)
>            vi <- readArray ar i
>            vj <- readArray ar j
>            writeArray ar j vi
>            return vj
>     where
>     n = length xs
>     newArray :: Int -> [a] -> IO (IOArray Int a)
>     newArray n xs = newListArray (1,n) xs

                
> mkGraph :: Int -> Int -> Int -> String -> IO ()
> mkGraph nl nu db out
>     = do n <- max 2 <$> randomRIO (nl, nu)
>          assert "split factor" $ db>0
>          q <- randomRIO (n, n*db)
>          let l = div q (db+1)
>              r = n - l
>          -- connect at least every node once
>          ys0 <- replicateM l $ randomRIO (0,r-1)
>          let ys1 = [y | y <- [0..r-1], not $ elem y ys0]
>          xs1 <- replicateM (length ys1) $ randomRIO (0,l-1)
>          let es0 = zip [0..l-1] ys0 ++ zip xs1 ys1
>          -- maybe add more edges
>          k <- randomRIO (0, l*r - length es0)
>          es1 <- shuffle k [(x,y) | x <- [0..l-1], y <- [0..r-1]]
>          let g = S.fromList . map (\(x,y) -> (2*x+1,2*y)) $ es0++es1
>
>          writeFile out . fmt $ S.toList g
>          let (es,ec,ls,rs,lc,rc) = stat g
>                                    
>          putStrLn $ unwords [show lc, show rc, show ec]
>          assert "left count" $ lc == l
>          assert "right count" $ rc == r
           
> fglMatcher ::S.Set (Int,Int) -> IO ()
> fglMatcher g
>     = do let (es,ec,ls,rs,lc,rc) = stat g
>          assert "bipartite" $ S.null (S.intersection ls rs)
>          let enter = map ((,) (-1)) $ S.toList ls
>              leave = map (flip (,) (-2)) $ S.toList rs
>              es' = map (\e -> (fst e, snd e, 1)) $ enter ++ leave ++ es
>              ns' = map (flip (,) ()) $ -1 : -2 : S.toList ls ++ S.toList rs
>              fglGraph = G.mkGraph ns' es' :: G.Gr () Int
>              maxFlow = G.maxFlow fglGraph (-1) (-2)
>          assert "edge count" $ length es' == ec + lc + rc
>          assert "node count" $ length ns' == lc + rc + 2
>          assert "fgl edge count" $ length (G.labEdges fglGraph) == ec + lc + rc
>          assert "fgl node count" $ G.noNodes fglGraph == lc + rc + 2
>          print maxFlow

> mine :: String -> S.Set (Int,Int) -> IO ()
> mine out g
>     = do let (es,ec,ls,rs,lc,rc) = stat g
>          assert "bipartite" $ S.null (S.intersection ls rs)
>          let m = matching g
>              ms = map swap $ M.toList m -- matchings
>              mc = M.size m
>          writeFile out $ fmt ms
>          assert "subset" $ S.fromList ms `S.isSubsetOf` g
>          assert "injective" $ S.size (S.fromList $ map snd ms) == mc
>          assert "max size" $ mc <= minimum [ec,lc,rc]
>          print mc

> main
>     = do as <- getArgs
>          case as of
>            ["fgl",gf] -> fglMatcher =<< parse <$> readFile gf
>                          
>            ["mine",gf] -> mine "/dev/null" =<< parse <$> readFile gf
>            ["mine",gf,mf] -> mine mf =<< parse <$> readFile gf
>                             
>            ["mkgraph",nl,nu,db,gf] -> mkGraph (read nl) (read nu) (read db) gf
>
>            _ -> putStrLn help

> help =
>     "usage : testMatcher <command>\n\
>     \command : fgl <in>\n\
>     \        | mine <in> <out>?\n\
>     \        | mkgraph <min nodes> <max nodes> <min balance> <out>\n\
>     \use with ‘time  -f '%e %M'’\n"
