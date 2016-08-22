
Command line tool to generate random bipartite graphs.

> module Main where

> import ArbGraph
> import qualified Data.Set as S
> import Data.List ( intersperse )
> import Control.Concurrent.ParallelIO.Global as P
> import Control.Concurrent ( setNumCapabilities )
> import Control.Monad ( when )
> import System.Directory ( createDirectoryIfMissing )
> import System.Environment ( getArgs )
> import System.Exit ( exitFailure )

> serialize :: Graph -> String
> serialize
>   = foldr ($) "" . intersperse (showChar '\n')
>     .
>     map (\(N a, N b) -> shows a . showChar ' ' . shows b)
>     .
>     S.toList



To get parallelism [1], compile with `-threaded`.  The number of
threads to actually use must be given as command line argument.

> main :: IO ()
> main
>   = do as <- getArgs 
>        case as of
>          (threads : dir : rounds : sizes) | not $ null sizes
>            -> do setNumCapabilities $ int threads
>                  when (dir /= "/dev/null") $
>                      createDirectoryIfMissing True dir
>                  P.parallel_  -- sequence_
>                    [ do g <- arbGraph s
>                         let m = S.size g
>                             (l,r) = (\(xs,ys)-> ( S.size (S.fromList xs)
>                                                 , S.size (S.fromList ys)
>                                                 )
>                                     ) . unzip $ S.toList g
>                             fn = concat
>                                  [ dir, "/g", show i, "-", show l, "l-"
>                                  , show r, "r-", show m, "e.graph"
>                                  ]
>                         when (dir /= "/dev/null") $
>                             writeFile fn $ serialize g
>                         putStrLn fn
>                    | s <- map int sizes
>                    , i <- take (int rounds) [1::Int ..]
>                    ]
>                  P.stopGlobalPool
>          _ -> do putStrLn help
>                  exitFailure

> int :: String -> Int
> int = read
>
> help :: String
> help
>   = "\n\
>     \mkGraphs <threads> <dir> <repeat> <size1> <size2> ...\n\
>     \\n\
>     \This will generate <repeat> many graphs of the given <size>s in\n\
>     \the <dir>ectory, using <threads> CPUs in parallel.\n\
>     \\n\
>     \Existing files will be overwritten.\n"

[1] https://hackage.haskell.org/package/parallel-io-0.3.3/docs/Control-Concurrent-ParallelIO-Global.html
