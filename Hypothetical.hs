{-# LANGUAGE NoMonomorphismRestriction #-}

module Hypothetical where

import           Data.List                      (permutations)
import           Data.List.Split                (chunksOf)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

type Dia = Diagram B
type Colors = [Colour Double]

colors :: Colors
colors = [red, green, blue, orange, purple]

drawDot :: Int -> Dia
drawDot = drawDot' colors

drawDot' :: Colors -> Int -> Dia
drawDot' cs i = circle 1 # lw none # fc (cs !! i) # named i

drawList :: [Int] -> Dia
drawList = drawList' colors

drawList' :: Colors -> [Int] -> Dia
drawList' cs = vsep 1 . map (drawDot' cs)

drawPerm :: [Int] -> [Int] -> Dia
drawPerm = drawPerm' colors

drawPerm' :: Colors -> [Int] -> [Int] -> Dia
drawPerm' cs p1 p2 =
  hsep 4 ["a" |> drawList' cs p1, "b" |> drawList' cs p2]
  # applyAll
    [ connect' (with & arrowHead .~ noHead & shaftStyle %~ lc (cs !! i)) ("a" .> i) ("b" .> i)
    | i <- [0 .. length p1 - 1]
    ]
  # lwL 0.2

-- This took me 9 minutes to produce.
perm1 = drawPerm [0,3,1,2] [0..3]

-- This took me an additional 2 mins 50 secs.
perms4 = perms4' colors

-- Change all the green to purple?  Took me a couple seconds.
perms4' cs = vsep 3 . map (hsep 4) . chunksOf 6 . map (drawPerm' cs [0..3]) . permutations $ [0..3]

perms4mod cs = vsep 4 . map (hsep 5) . chunksOf 6 . map (rotateBy (1/4) . drawPerm' cs [0..3]) . permutations $ [0..3]
