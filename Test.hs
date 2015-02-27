{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude               hiding (dot)

dia = vsep 1 (map centerX [ptVec, dirAng, pt])

ptVec = hcat [ circle 0.1 # fc black, strutX 3, arrowV (4 ^& 3) # centerY # scale (3/4) ]

dirAng = hcat [ arrowV (4 ^& 3) # scale (3/4), strutX 3, ang # rotateBy (1/18)]
  where
    ang = mconcat
      [ arrowV' (with & arrowHead .~ noHead) unitX # scale 3
      , arrowV' (with & arrowHead .~ noHead) (normalize $ 4 ^& 3) # scale 3
      , arc xDir (angleBetween unitX (4 ^& 3 :: V2 Double))
      ]

dot = circle 0.1 # fc black

pt = hcat [ strokeTrail theTrail <> dot <> dot # translate (trailOffset theTrail)
          , strutX 3
          , arrowV' (with & arrowShaft .~ theTrail) (trailOffset theTrail)
          ]
  where
    theTrail = fromOffsets [1 ^& 1, 2 ^& (-0.5), 1 ^& 1]

main :: IO ()
main = defaultMain (dia # frame 0.5)
