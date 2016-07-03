> module Main ( main ) where

> import Data.Graph.MaxBipartiteMatching
> import qualified Data.Set as S
> import qualified Data.Map as M
> import System.Environment ( getArgs )


> from :: FilePath -> IO String
> from "-" = getContents
> from fn = readFile fn

> to :: FilePath -> String -> IO ()
> to "-" = putStr
> to fn = writeFile fn

> main :: IO ()
> main
>     = do as <- getArgs
>          case as of
>            [i] -> putStrLn . show . M.size . m =<< from i
>            [i,o] -> to o . unlines . map (\(a,b) -> a++" "++b) . M.toList . m
>                     =<< from i
>            other -> putStr help
>     where
>     m = matching . S.fromList . pairs . lines
>     pairs (l:ls) = case words l of
>                      (x:y:_) -> (y,x) : pairs ls
>                      _ -> pairs ls
>     pairs [] = []

> help :: String
> help =
>  "\n\
>  \Name: matcher — Maximum cardinality bipartite matching\n\
>  \\n\
>  \Synopsis: matcher [‹in› [‹out›]]\n\
>  \\n\
>  \Without arguments, show this help, otherwise read graph from file ‹in›.\n\
>  \If ‹out› is provided, write the calculated matching to file ‹out›,\n\
>  \otherwise print the size of the matching.  The files can be specified as\n\
>  \`-` to refer to stdin/stdout respectively.\n\
>  \\n\
>  \The input file is split into lines to denote edges.  The first/second\n\
>  \word on each line is considered a left/right node, the rest of the line\n\
>  \is ignored.  Lines with less than two words are ignored.  Note, that\n\
>  \the graph is simple, and that left and right nodes use different\n\
>  \namespaces, i.e.,\n\
>  \\n\
>  \    A B\n\
>  \    A B    -- ignore multiple edges\n\
>  \    A C\n\
>  \    B A    -- left X ≠ right X\n\
>  \\n\
>  \denotes a graph with\n\
>  \\n\
>  \    #V = #{A,A,A,B} + #{B,B,C,A} = 2 + 3 = 5  nodes, and\n\
>  \\n\
>  \    #E = #{(A,B),(A,B),(A,C),(B,A)} = 3  edges.\n\
>  \\n\
>  \A maximum cardinality bipartite matching is {(A,B),(B,A)}.\n\
>  \\n"
