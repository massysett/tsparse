module Main where

import qualified TsParse as T
import qualified Test.QuickCheck.Gen as G
import qualified System.Random as R
import qualified Text.PrettyPrint as P
import System.Environment (getArgs)

main :: IO ()
main = do
  size:[] <- fmap (map read) getArgs
  gen <- R.getStdGen
  let rend = G.unGen T.genTspStatement gen size
      pretty = P.render . T.pretty . T.ast $ rend
  putStrLn pretty
  putStrLn (T.rendering rend)
