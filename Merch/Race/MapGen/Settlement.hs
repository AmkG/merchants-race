{- Merch.Race.MapGen.Settlement - Generates settlements and the road network.

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
{- Generate settlements and roads.  -}

module Merch.Race.MapGen.Settlement
  ( drawSettlement
  ) where

import Merch.Race.Data
import Merch.Race.Hex
import Merch.Race.MapGen.Monad
import Merch.Race.MapGen.Substep

import Control.Monad
import Data.Graph.AStar
import Data.Ix
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Ratio
import qualified Data.Set as Set
import Data.Set(Set)

pickName :: MapGenM m => NameGenerator -> m String
pickName (NGString s)        = return s
pickName (NGConcat ngs)      = do
  names <- mapM pickName ngs
  return $ concat names
pickName (NGDistribute ngis) = do
  let total = sum $ map snd ngis
  ri <- mgRandom
  let i = abs ri `mod` total
      findLoop i ((ng, n):ngis)
        | i < n     = ng
        | otherwise = findLoop (i - n) ngis
  pickName $ findLoop i ngis

loop :: MapGenM m
     => NameGenerator --              Name generator.
     -> Int --                        Total number of settlements to generate.
     -> Map Settlement HexCoord --    Map of already-generated settlements.
     -> Map Terrain (Set HexCoord) -- Terrain locations.
     -> Int --                        Number of generated settlements.
     -> [SettlementType] --           List of settlement types to generate.
     -> m [(Settlement, HexCoord)]
loop ng total = core
 where
   core done tm numGen []       = mgProgress 1 >> (return $ Map.toList done)
   core done tm numGen (st:sts) = do
     mgProgress $ fromIntegral numGen % fromIntegral total
     -- Look for candidate locations
     ts <- mgRequiredTerrain st
     let hs = concatMap (Set.toList . maybe Set.empty id . flip Map.lookup tm) ts
         lengthHs = length hs
     -- Select a candidate location.
     ri <- mgRandom
     let i = abs ri `mod` lengthHs
         h = hs !! i
     -- Pick a name.
     let pickloop = do
         name <- pickName ng
         let settlement = Settlement name
         if Map.member settlement done
          then pickloop
          else return settlement
     s <- pickloop
     -- Place the item.
     mgAddSettlement s st h
     let done' = Map.insert s h done
     -- Create a road if needed.
     when (not $ Map.null done) $ do
       -- Select a random, already-done settlement.
       let sizeDone = Map.size done
       ri <- mgRandom
       let i = abs ri `mod` sizeDone
           (s2, h2) = Map.toList done !! i
       -- Get the path to its location.
       let neighborM h = do
             let ns = neighbors h
             ns' <- filterM (\n -> do t <- mgGetTerrain n
                                      return $ not $ any (==t) [Sea,Freshwater])
                            ns
             return $ Set.fromList ns'
           movecost :: Terrain -> Rational
           movecost Sea        = 100000
           movecost Freshwater = 100000
           movecost Coast      = 10
           movecost Plains     = 1.7
           movecost Forest     = 5
           movecost Hill       = 5.5
           movecost Mountain   = 7
           movecostM h1 h2 = do
             -- For road to road, only 1
             r1 <- mgGetRoad h1
             r2 <- mgGetRoad h2
             if r1 && r2
              then return 1
              else do
                -- Use the larger movement cost.
                t1 <- mgGetTerrain h1
                t2 <- mgGetTerrain h2
                -- Adjust movement costs if going
                -- to a tile that is different y-coord
                -- in offset coordinates
                let (_, y1) = toOffset h1
                    (_, y2) = toOffset h2
                    (c1, c2) = (movecost t1, movecost t2)
                    (c1', c2')
                      | y1 == y2  = (c1, c2)
                      | otherwise = (c1 * 1.05, c2 * 1.05)
                return $ max c1' c2'
       Just roads <- aStarM
                       neighborM
                       movecostM
                       (return . fromIntegral . distance h)
                       (return . (==h))
                       (return h2)
       -- Fill in roads
       mapM_ (flip mgPutRoad True) roads
       mgPutRoad h True
       mgPutRoad h2 True

     -- Disallow tiles within 5 tiles of the selected tile.
     let disalloweds = Set.fromList $ nearby 5 h
         tm' = Map.map (Set.\\ disalloweds) tm

     core done' tm' (numGen+1) sts

drawSettlement :: MapGenM m => m [(Settlement, HexCoord)]
drawSettlement = do

  -- First collect the terrains.
  mgStep "Preparing settlement generation"
  b <- mgMapBounds
  let total = fromIntegral $ rangeSize b
  ths <- substep 0.00 0.10 $ forM (zip [0..] $ range b) $ \ (i, h) -> do
    mgProgress $ i % total
    t <- mgGetTerrain h
    return (t,Set.singleton h)
  let tm = foldl' (flip $ uncurry $ Map.insertWith Set.union) Map.empty ths

  mgStep "Placing settlements"
  ng <- mgNameGenerator
  ists <- mgSettlementGenerator
  let sts = concatMap (uncurry replicate) ists
      total = length sts
  substep 0.10 0.90 $ loop ng total Map.empty tm 0 sts

