{-# LANGUAGE NoMonomorphismRestriction #-}

module Icons where

import           Control.Arrow                  (second)
import           Control.Lens hiding ((#), none)
import           Data.List                      (transpose)
import           Data.List.Split
import qualified Data.Map                       as M
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

type Dia = Diagram B

weights :: [Double] -> Dia
weights hs
  = hs
  # map (\h -> roundedRect 1 h 0.2)
  # hsep 0.3

dumbbell :: Double -> [Double] -> Dia
dumbbell w hs = hsep 0.5 [cap, wts, bar, wts # reflectX, cap]
  where
    wts = weights hs
    cap = rect 0.8 1
    bar = rect w 1

power :: Dia
power = dumbbell 3 [4,5] # lw none

flexibility :: Dia
flexibility = strokeTrail . closeTrail $ catenary 2 <> vrule 0.1 <> catenary 1.8 # reverseTrail

catenary :: Double -> Trail V2 Double
catenary 0 = hrule 2
catenary a = cubicSpline False [x ^& (0.5 * a * (exp (x/a) + exp (-x/a))) | x <- [-1, -0.9 .. 1]]

learning :: Dia
learning = mconcat [axis, rotateBy (1/4) axis, curve] # lw thick
  where
    axis = hrule 2 # alignL # translateX (-0.2)
    curve = cubicSpline False [x ^& (32 / 45 * x * x + 0.2) | x <- [0, 0.1 .. 1.5]]  -- just a parabola

repetition :: Dia
repetition = hsep 0.5 . map (vsep 0.5) . chunksOf 3 $ replicate 9 dot
  where
    dot = circle 0.5

programmability :: Dia
programmability = vsep keygap . map row $ keys
  where
    keys = [ [1,1,1,1,1]
           , [1,1,1,2  ]
           , [1,  3  ,1]
           ]
    row = hsep keygap . map mkKey
    mkKey w = roundedRect (w - keygap) (1 - keygap) keyround
    keygap = 0.3
    keyround = 0.1

modification :: Dia
modification = lw none . hsep 0.5 . map (vsep 0.5) . chunksOf 3
             $ dot : concat (replicate 3 [dot, triangle 1]) ++ replicate 2 dot
  where
    dot = circle 0.5
    gdot = dot # fc green

face :: Double -> Colour Double -> Dia
face mouthDir color
  = mconcat
    [ eye # translateX (-2/5)
    , eye # translateX (2/5)
    , mouth
    , circle 1
    ]
    # lc color
    # fc white
  where
    eye = circle (1/7) # fc color # lw none
    mouth = catenary mouthDir # scale (1/2) # strokeTrail # centerXY # translateY (-1/2)

happy, sad :: Dia
happy = face 1 green
meh   = face 0 orange
sad   = face (-1) red

drawTable :: [[Dia]] -> Dia
drawTable = vsep 0.5 . map (hsep 1) . (map . map) (centerXY . sized (mkSizeSpec (Just 1 ^& Nothing))) . transpose . lc blue . fc blue

criteria
  = M.fromList $
    [ ("power", power)
    , ("flexibility", flexibility)
    , ("learning", learning)
    , ("repetition", repetition)
    , ("programmability", programmability)
    , ("modification", modification)
    ] # (map . second) (fc blue . lc blue)

allCriteria :: Dia
allCriteria = hsep 0.5 . map (centerXY . sized (mkSizeSpec (Just 1 ^& Nothing))) $ M.elems criteria

criteriaTable :: Dia
criteriaTable =
  drawTable
  [ [power           ]
  , [flexibility     ]
  , [learning        ]
  , [programmability ]
  ]
      # frame 0.5

testDia = -- hsep 0.5 . map (centerXY . sized (mkSizeSpec (Just 1 ^& Nothing))) $
         -- [power, flexibility, learning, repetition, programmability, modification]
         -- [happy, sad]
         drawTable
           [ [power           , happy ]
           , [flexibility     , sad   ]
           , [learning        , happy ]
           , [repetition      , happy ]
           , [programmability , sad   ]
           , [modification    , sad   ]
           ]

theTable =
  [ [label "" , label "Tools" , label "GP Langs" , label "DSLs" , label "EDSLs" ]
  , [power           , happy , sad   , happy , happy ]
  , [flexibility     , meh   , happy , happy , happy ]
  , [learning        , happy , sad   , meh   , meh   ]
  , [programmability , sad   , happy , meh   , happy ] 
  ]
  where
    label s = text s # fontSizeL 0.6 # fc black <> rect 2 1 # lw none # fc white

hideRows rs tab = tab # transpose # applyAll (map hideRow rs) # transpose
  where
    hideRow r tab = tab & ix r %~ (applyAll (map hideOne [1..4]))
    hideOne i row = row & ix i %~ (\d -> rect (width d * 1.05) (height d * 1.05) # fc white # lw none <> d)

main :: IO ()
main = defaultMain $ testDia # lc blue # fc blue # frame 0.5
