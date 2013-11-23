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
  return $ foldl' (\r d -> d r) emptyRuleset ds

whitespace :: Parser ()
whitespace = lineDirective
         <|> (many space >> return ())
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
-- TODO: other top declarations

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
terrain = (keyword "sea"        >> return Sea)
      <|> (keyword "freshwater" >> return Freshwater)
      <|> (keyword "coast"      >> return Coast)
      <|> (keyword "plains"     >> return Plains)
      <|> (keyword "forest"     >> return Forest)
      <|> (keyword "hill"       >> return Hill)
      <|> (keyword "mountain"   >> return Mountain)
