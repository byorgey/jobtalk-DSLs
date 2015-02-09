{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List
import Data.List.Split
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

type Dia = Diagram B

colors = [red, green, blue, orange]

drawDot :: Int -> Dia
drawDot i = circle 1 # lw none # fc (colors !! i) # named i

drawList :: [Int] -> Dia
drawList is = vsep 0.5 (map drawDot is)
-- drawList = vsep 0.5 . map drawDot

drawPerm :: [Int] -> [Int] -> Dia
drawPerm is js =
  (("a" |> drawList is) ||| strutX 4 ||| ("b" |> drawList js))
  # applyAll [ connect' (connectStyle i) ("a" .> i) ("b" .> i) | i <- [0 .. length is - 1] ]

connectStyle i = with & arrowHead .~ noHead & shaftStyle %~ lc (colors !! i)

drawAllPerms =
    vsep 2
  . map (hsep 2)
  . chunksOf 6
  . zipWith drawPerm (repeat [0..3])
  . permutations
  $ [0..3]

main = defaultMain (drawAllPerms)
