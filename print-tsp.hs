module Main where

import qualified TsParse as T
import Text.PrettyPrint (render)
import System.Environment (getArgs)

main :: IO ()
main = do
  x:[] <- getArgs
  stmt <- T.parseTspFromFile x
  putStr . render . T.pretty $ stmt
