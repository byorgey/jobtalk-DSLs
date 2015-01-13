{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List.Split
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

weights :: [Double] -> Diagram B R2
weights hs
  = hs
  # map (\h -> roundedRect 1 h 0.2)
  # hcat' (with & sep .~ 0.3)

dumbbell :: Double -> [Double] -> Diagram B R2
dumbbell w hs = hcat' (with & sep .~ 0.5) [cap, wts, bar, wts # reflectX, cap]
  where
    wts = weights hs
    cap = rect 0.8 1
    bar = rect w 1

power :: Diagram B R2
power = dumbbell 3 [4,5] # lw none

flexibility :: Diagram B R2
flexibility = strokeTrail . closeTrail $ catenary 2 <> vrule 0.1 <> catenary 1.8 # reverseTrail
  where
    catenary a = cubicSpline False [x ^& (0.5 * a * (exp (x/a) + exp (-x/a))) | x <- [-1, -0.9 .. 1]]

learning :: Diagram B R2
learning = mconcat [axis, rotateBy (1/4) axis, curve] # lw thick
  where
    axis = hrule 2 # alignL # translateX (-0.2)
    curve = cubicSpline False [x ^& (32 / 45 * x * x + 0.2) | x <- [0, 0.1 .. 1.5]]  -- just a parabola

repetition :: Diagram B R2
repetition = hcat' (with & sep .~ 0.5) . map (vcat' (with & sep .~ 0.5)) . chunksOf 3 $ replicate 9 dot
  where
    dot = circle 0.5

programmability :: Diagram B R2
programmability = vcat' (with & sep .~ keygap) . map row $ keys
  where
    keys = [ [1,1,1,1,1]
           , [1,1,1,2  ]
           , [1,  3  ,1]
           ]
    row = hcat' (with & sep .~ keygap) . map mkKey
    mkKey w = roundedRect (w - keygap) (1 - keygap) keyround
    keygap = 0.25
    keyround = 0.1

modification :: Diagram B R2
modification = lw none . hcat' (with & sep .~ 0.5) . map (vcat' (with & sep .~ 0.5)) . chunksOf 3
             $ dot : concat (replicate 3 [dot, triangle 1]) ++ replicate 2 dot
  where
    dot = circle 0.5
    gdot = dot # fc green

dia = (hcat' (with & sep .~ 0.5) . map (centerXY . sized (Width 1)))
        [power, flexibility, learning, repetition, programmability, modification]

main :: IO ()
main = defaultMain $ dia # lc blue # fc blue # frame 0.5
