{- Merch.Race.Ruleset.Parser - Parser for game rulesets.

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
{- Parser for rulesets.  -}

module Merch.Race.Ruleset.Parser
  ( parseRuleset
  ) where

import Merch.Race.Data
import Merch.Race.Ruleset.Data

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Ratio
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

parseRuleset :: Parser Ruleset
parseRuleset = do
  whitespace
  ds <- many (enwhitespace topDecl)
  eof
  return $ foldl' (\r d -> d r) emptyRuleset ds

whitespace :: Parser ()
whitespace = do
  many ((space >> return ()) <|> lineDirective)
  return ()
 <?> "whitespace"
 where
  lineDirective = do
    keyword "#line"
    many space
    l <- int
    many space
    s <- cstring
    many horizspace
    newline
    setPosition $ newPos s l 1
    return ()
  space = newline <|> horizspace
  horizspace = satisfy (\c -> any (c==) " \t\v\r")
  newline = char '\n'

cstring :: Parser String
cstring = do
  char '"'
  cs <- many cstringchar
  char '"'
  return cs
 <?> "string"
 where
  cstringchar :: Parser Char
  cstringchar = escapeChar
            <|> satisfy (/= '"')
            <?> "string character"
  escapeChar = do
    char '\\'
    (    (char 'a' >> return '\a')
     <|> (char 'b' >> return '\b')
     <|> (char 'f' >> return '\f')
     <|> (char 'n' >> return '\n')
     <|> (char 'r' >> return '\r')
     <|> (char 't' >> return '\t')
     <|> (char 'v' >> return '\v')
     <|> (char '"' >> return '\"')
     <|> (char '\\' >> return '\\')
     )

int :: Parser Int
int = do
  cs <- many1 digit
  return $ read cs
 <?> "integer"

ratio :: Parser Rational
ratio = do
  n <- numberPart
  p <- percentPart
  return $ p n
 <?> "fractional number"
 where
  numberPart = hasWhole
           <|> fractional
  hasWhole = do
    n <- int
    n2 <- fractionalPart
    return $ fromIntegral n + n2
  fractionalPart = fractional
               <|> noFractional
  noFractional = return 0.0
  fractional = do
    char '.'
    zs <- many $ char '0'
    let zeros = length zs
    cs <- many1 digit
    let digits = length cs + zeros
        num = read cs :: Integer
    return $ num % (10 ^ fromIntegral digits)
  percentPart = try hasPercent
            <|> noPercent
  hasPercent = do
    whitespace
    char '%'
    return $ \n -> n / 100
  noPercent = return $ id

keyword :: String -> Parser ()
keyword s = try $ do
  string s
  notFollowedBy (satisfy (\c -> isAlphaNum c || c == '_'))
  return ()

enwhitespace :: Parser a -> Parser a
enwhitespace p = do
  rv <- p
  whitespace
  return rv

type TopDeclaration = Ruleset -> Ruleset

topDecl :: Parser TopDeclaration
topDecl = itemDecl
      <|> settlementTypeDecl
      <|> nameGeneratorDecl
      <|> settlementGeneratorDecl
      <|> difficultyDecl

itemDecl = do
  enwhitespace $ keyword "item"
  name <- enwhitespace $ cstring
  enwhitespace $ char '{'
  desc <- enwhitespace $ description
  char '}'
  return $ \r -> r { itemmap = Map.insert (Item name) desc (itemmap r)}
 <?> "item"

settlementTypeDecl = do
  enwhitespace $ keyword "settlementType"
  name <- enwhitespace $ cstring
  enwhitespace $ char '{'
  decls <- many $ enwhitespace subDecl
  char '}'
  let raw = emptySettlementTypeDesc
      named = raw { stname = name }
      final = foldl' (\r d -> d r) named decls
      convname = SettlementType name
  return $ \r ->
    r { settlementtypemap = Map.insert convname final $ settlementtypemap r }
 <?> "settlementType"
 where
  subDecl = stDesc
        <|> producers
        <|> consumers
        <|> craftsmen
        <|> terrainDecl
  stDesc = do
    s <- description
    return $ \d -> d { stdesc = s }
  prodcons kw f = do
    enwhitespace $ keyword kw
    enwhitespace $ char '{'
    ps <- many $ enwhitespace prodCons
    char '}'
    return $ f ps
  producers = prodcons "producers" $ \ps d -> d { stproducers = ps }
  consumers = prodcons "consumers" $ \ps d -> d { stconsumers = ps }
  craftsmen = do
    enwhitespace $ keyword "craftsmen"
    enwhitespace $ char '{'
    cs <- many $ enwhitespace craftsman
    char '}'
    return $ \d -> d { stcraftsmen = cs }
  terrainDecl = do
    enwhitespace $ keyword "terrain"
    enwhitespace $ char '{'
    ts <- many1 $ enwhitespace terrain
    char '}'
    return $ \d -> d { stterrain = ts }
nameGeneratorDecl = do
  enwhitespace $ keyword "nameGenerator"
  ng <- nameGenerator
  return $ \r -> r { namegeneratormaybe = Just ng }
settlementGeneratorDecl = do
  enwhitespace $ keyword "settlementGenerator"
  enwhitespace $ char '{'
  ists <- many $ enwhitespace $ do
    num <- enwhitespace int
    st <- enwhitespace cstring
    return $ (num, SettlementType st)
  char '}'
  return $ \r -> r { settlementgeneratorlist = ists }
difficultyDecl = do
  enwhitespace $ keyword "difficulty"
  name <- enwhitespace cstring
  let dname = Difficulty name
  enwhitespace $ char '{'
  ds <- many $ enwhitespace subDecl
  char '}'
  let raw = emptyDifficultyDesc
      named = raw { difficultyname = name }
      final = foldl' (\d f -> f d) named ds
  return $ \r -> r { difficultymap = Map.insert dname final $ difficultymap r }
 where
  subDecl = ddesc
        <|> cartUpgradeCost
        <|> supplyCost
        <|> innCost
        <|> marketLunchCost
        <|> pubLunchCost
        <|> banditProbability
        <|> permeabilityDecl
        <|> interestRate
        <|> priceSettings
        <|> startingLoan

  ddesc = do
    s <- description
    return $ \d -> d { difficultydesc = s }
  costing kw func = do
    enwhitespace $ keyword kw
    r <- enwhitespace $ ratio
    is <- itemSet
    return $ func (r, is)
  cartUpgradeCost =
    costing "cartUpgradeCost" $ \r d -> d {cartupgradecost = r}
  supplyCost =
    costing "supplyCost" $ \r d -> d {supplycost = r}
  innCost =
    costing "innCost" $ \r d -> d {inncost = r}
  marketLunchCost =
    costing "marketLunchCost" $ \r d -> d {marketlunchcost = r}
  pubLunchCost =
    costing "pubLunchCost" $ \r d -> d {publunchcost = r}
  banditProbability = do
    enwhitespace $ keyword "banditProbability"
    enwhitespace $ char '{'
    ls <- many $ enwhitespace $ do
      t <- enwhitespace terrain
      r <- ratio
      return $ (t, r)
    char '}'
    return $ \d -> d {banditprobability = Map.fromList ls}
  ratioDecl kw func = do
    enwhitespace $ keyword kw
    r <- ratio
    return $ func r
  permeabilityDecl =
    ratioDecl "permeability" $ \r d -> d {permeability = r}
  interestRate =
    ratioDecl "interestRate" $ \r d -> d {interestrate = r}
  priceSettings = do
    enwhitespace $ keyword "priceSettings"
    cp <- enwhitespace int
    kp <- enwhitespace ratio
    ki <- enwhitespace ratio
    kd <- ratio
    return $ \d -> d { centerprice = fromIntegral cp
                     , pidsettings = (kp,ki,kd)
                     }
  startingLoan = do
    enwhitespace $ keyword "startingLoan"
    l <- int
    return $ \d -> d { startingloan = fromIntegral l }

description = do
  enwhitespace $ keyword "description"
  enwhitespace $ char '{'
  ss <- many $ enwhitespace cstring
  char '}'
  return $ concat ss
 <?> "description"
prodCons = scheduled
       <|> probability
       <?> "producer/consumer"
 where
  {-
  scheduled day +/- day itemset
  e.g.
    scheduled 300 +/-1 {3 "Grain"}
  -}
  scheduled = do
    enwhitespace $ keyword "scheduled"
    day <- enwhitespace $ int
    enwhitespace $ keyword "+/-"
    offset <- enwhitespace $ int
    is <- itemSet
    return $ Scheduled (fromIntegral day) (fromIntegral offset) is
  {-
  probability prob itemset
  e.g.
    probability 10% {1 "Dried Fish"}
  -}
  probability = do
    enwhitespace $ keyword "probability"
    prob <- enwhitespace ratio
    is <- itemSet
    return $ Probability prob is
itemSet = anyItemSet
      <|> directItemSet
      <?> "itemset"
 where
  anyItemSet = do
    enwhitespace $ keyword "any"
    enwhitespace $ char '{'
    is <- enwhitespace directItemSet
    iss <- many $ enwhitespace directItemSet
    char '}'
    return $ foldl' (\(ItemSet s ss) (ItemSet s2 _) ->
                     (ItemSet s (s2:ss)))
                    is iss
  directItemSet = do
    enwhitespace $ char '{'
    iis <- many $ enwhitespace intItem
    char '}'
    return $ ItemSet iis []
  intItem = do
    num <- enwhitespace int
    item <- cstring
    return $ (fromIntegral num, Item item)
craftsman = do
  inp <- enwhitespace itemSet
  enwhitespace $ string "->"
  out <- itemSet
  return $ Craftsman inp out
 <?> "craftsman"
terrain = (keyword "sea"        >> return Sea)
      <|> (keyword "freshwater" >> return Freshwater)
      <|> (keyword "coast"      >> return Coast)
      <|> (keyword "plains"     >> return Plains)
      <|> (keyword "forest"     >> return Forest)
      <|> (keyword "hill"       >> return Hill)
      <|> (keyword "mountain"   >> return Mountain)
nameGenerator = ngstring
            <|> ngconcat
            <|> ngany
            <|> distribute
            <?> "name generator"
 where
  ngstring = do
    s <- cstring
    return $ NGString s
  ngconcat = do
    enwhitespace $ keyword "concat"
    enwhitespace $ char '{'
    ngs <- many $ enwhitespace nameGenerator
    char '}'
    return $ NGConcat ngs
  ngany = do
    enwhitespace $ keyword "any"
    enwhitespace $ char '{'
    ngs <- many $ enwhitespace nameGenerator
    char '}'
    let ones = 1:ones
    return $ NGDistribute $ zip ngs ones
  distribute = do
    enwhitespace $ keyword "distribute"
    enwhitespace $ char '{'
    ngis <- many $ enwhitespace $ do
      i <- enwhitespace int
      ng <- nameGenerator
      return (ng, i)
    char '}'
    return  $ NGDistribute ngis
