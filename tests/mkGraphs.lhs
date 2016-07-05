> module Main where

> import ArbGraph
> import qualified Data.Set as S
> import Data.List ( intersperse )
> import Control.Concurrent.ParallelIO.Global as P
> import Control.Concurrent ( setNumCapabilities )
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



To get parallelism [1], compile with `-threaded`, and run with

    ./mkGraphs +RTS -N$(nproc) -RTS

> main :: IO ()
> main
>   = do as <- getArgs 
>        case as of
>          (threads : dir : rounds : sizes) | not $ null sizes
>            -> do setNumCapabilities $ int threads
>                  createDirectoryIfMissing True dir
>                  P.parallel_  -- sequence_
>                    [ serialize <$> arbGraph n >>= writeFile fn >> putStrLn fn
>                    | n <- map int sizes
>                    , r <- take (int rounds) [1::Int ..]
>                    , let fn = concat [dir, "/", show n, "-", show r, ".txt"]
>                    ]
>                  P.stopGlobalPool
>          _ -> do putStrLn help
>                  exitFailure
>               

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
