module Main where

import qualified TsParse as T
import Text.PrettyPrint (render)
import System.Environment (getArgs)
import Text.Parsec (parse)

main :: IO ()
main = do
  as <- getArgs
  (fn, txt) <- case as of
    x:[] -> do
      contents <- readFile x
      return (x, contents)
    [] -> do
      contents <- getContents
      return ("<stdin>", contents)
    _ -> error "cannot deal with more than one argument"
  case parse T.parseTsp fn txt of
    Left err -> fail . show $ err
    Right stmt -> putStrLn . render . T.pretty $ stmt
