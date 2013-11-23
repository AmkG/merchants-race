
{- Module to load a ruleset.  -}
module Merch.Race.Ruleset.Load
  ( loadRuleset
  ) where

import Merch.Race.Ruleset.Data
import Merch.Race.Ruleset.Parser

import Language.Preprocessor.Cpphs
import System.IO
import Text.ParserCombinators.Parsec

-- Given a directory path containing a ruleset.txt file,
-- load the ruleset.txt file.
loadRuleset :: FilePath -> IO Ruleset
loadRuleset dir = do
  let filename = dir ++ "/ruleset.txt"
      Right opts = parseOptions ["--strip", "-I"++dir]
  h <- openFile filename ReadMode
  dat <- hGetContents h
  pdat <- runCpphs opts filename dat
  putStrLn $ pdat
  case parse parseRuleset filename pdat of
    Left e  -> fail $ show e
    Right r -> return r
