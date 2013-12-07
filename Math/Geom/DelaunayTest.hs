{- Math.Geom.DelaunayTest - Test for Delaunay algorithm using quad-edge data structure.

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
module Math.Geom.DelaunayTest
  ( delaunayTest
  , delaunayTestRandom
  ) where

import Math.Geom.QuadEdge.Pure hiding (flip)
import Math.Geom.Delaunay

import Control.Monad
import Control.Monad.ST
import Data.List
import qualified Data.Set as Set
import Data.Ratio
import Data.Set(Set)
import System.IO
import System.Random

delaunayTest :: [(Rational, Rational)] -> IO ()
delaunayTest ps = do
  let (qep, l) = runST (do (qeds, l, _) <- delaunay ps
                           qep <- freezeQE qeds
                           return (qep, l))
  dumpDelaunaySVG qep l

dumpDelaunaySVG :: QEP (Rational, Rational) -> EdgeRef -> IO ()
dumpDelaunaySVG qep l = do
  out <- openFile "delaunay.svg" WriteMode
  hPutStrLn out "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
  hPutStrLn out "<svg width=\"200\" height=\"200\" id=\"svg2\" version=\"1.1\">"
  hPutStrLn out "<g id=\"layer1\">"
  hPutStrLn out "  <!-- Background white -->"
  hPutStrLn out "  <rect width=\"200\" height=\"200\" style=\"fill:#ffffff;stroke-width:0\" />"
  dumpAll out qep l Set.empty []
  hPutStrLn out "</g>"
  hPutStrLn out "</svg>"
  hClose out
 where
  dumpAll out qep edge outputted todo = do
    let edges = sameOrigin qep edge
        segments = map (lineSegment qep) edges
        edgesegments = zip edges segments
        -- filter out segments already in
        -- outputted.
        filtered_edgesegments = [(e, s) | (e, s) <- edgesegments
                                        , not $ Set.member s outputted]
        -- add targets to future work
        next_todo = map (sym . fst) filtered_edgesegments ++ todo
        -- get the segments for output
        filtered_segments = map snd filtered_edgesegments
        -- add segments to outputted
        next_outputted = foldl' (flip Set.insert) outputted filtered_segments
    -- output the segments
    forM_ filtered_segments $ \((x0, y0), (x1, y1)) -> do
      let x0d = fromRational x0 :: Double
          y0d = fromRational y0 :: Double
          x1d = fromRational x1 :: Double
          y1d = fromRational y1 :: Double
      hPutStrLn out $ "  <path"
      hPutStrLn out $ "    style=\"fill:none;stroke:#000000;stroke-width:1px;stroke-opacity:1\""
      hPutStrLn out $ "    d=\"M "++show x0d++ ","++show y0d++" "++show x1d++","++show y1d++"\""
      hPutStrLn out $ "    />"
    -- check further outputs
    case next_todo of
      next_edge:todo -> dumpAll out qep next_edge next_outputted todo
      []             -> return ()
  -- get all edges that have the same origin as the given
  -- edge, including the given edge.
  sameOrigin qep src = src:loop (onext qep src)
   where
    loop e
      | e == src  = []
      | otherwise = e:loop (onext qep e)
  -- normalize a line segment: given a segment x0,y0 to x1,y1,
  -- ensure that either x0 < x1, or x0 == x1 and y0 < y1
  normalize orig@((x0, y0), (x1, y1))
    | x0 < x1   = orig
    | x0 > x1   = ((x1, y1), (x0, y0))
    | y0 < y1   = orig
    | otherwise = ((x1, y1), (x0, y0))
  -- extract the line segment of a Delaunay edge.
  lineSegment qep e = normalize ((x0, y0), (x1, y1))
   where
    (x0, y0) = getData qep e
    (x1, y1) = getData qep (sym e)

delaunayTestRandom :: Int -> IO ()
delaunayTestRandom i = do
  seed1 <- randomIO
  seed2 <- randomIO
  let rand1 = mkStdGen seed1
      rand2 = mkStdGen seed2
      xsi :: [Integer]
      xsi = randomRs (0, 20000) rand1
      ysi :: [Integer]
      ysi = randomRs (0, 20000) rand2
      xs :: [Rational]
      xs = map (% 100) xsi
      ys :: [Rational]
      ys = map (% 100) ysi
      ps = take i $ zip xs ys
  when (i < 16) $ putStrLn $ show $ map (\ (x,y) -> (fromRational x, fromRational y)) ps
  delaunayTest ps
