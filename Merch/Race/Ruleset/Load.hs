{- Merch.Race.Ruleset.Load - Ruleset loader.

Copyright 2013 Alan Manuel K. Gloria

This file is part of Merchant's Race.

Merchant's Race is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Merchant's Race is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Merchant's Race.  If not, see <http://www.gnu.org/licenses/>.
-}
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
  case parse parseRuleset filename pdat of
    Left e  -> fail $ show e
    Right r -> return r
