{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified System.Exit as Exit
import qualified TsParse as T

main :: IO ()
main = T.runAllTests >>= \b ->
  if b then Exit.exitSuccess else Exit.exitFailure
