{-# LANGUAGE NoMonomorphismRestriction #-}

module GUI where

import Control.Arrow ((***))
import Data.Char (isSpace)
import Data.List (genericLength)
import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

type Dia = Diagram B

enframe' :: Double -> Double -> Dia -> Dia
enframe' w h d = d # centerXY # sizedAs (r # scale 0.9) <> r
  where
    r :: Dia
    r = rect w h

enframe :: Dia -> Dia
enframe = enframe' 2 3

guiFrame :: Dia -> Dia -> Dia
guiFrame d1 d2 = (enframe d1 ||| enframe d2) # centerX

mouseCursor :: Dia
mouseCursor = arrowV ((-1) ^& 3) # translate (1 ^& (-3)) # scale 0.17 # lw thick

menuItem w txt = (alignedText 0 0.5 txt # scale 0.08 <> rect w 0.2 # fc lightgrey # lw thin # alignX (-0.9)) # centerX

menu w = vcat . map (alignL . menuItem w)

menuBar = menuItem 4 "File       Edit       Options       Ponies       Unicorns       Help"

textize :: String -> Dia
textize
  = vcat' (with & catMethod .~ Distrib & sep .~ 2)
  . map
    ( uncurry (|||)
    . (strutX *** (alignL . hrule))
    . (genericLength *** genericLength)
    . span isSpace
    )
  . lines

wiggly :: Double -> Double -> Trail V2 Double -> Trail V2 Double
wiggly n m tr
  = cubicSpline False
  . map (\(t, off, norm) -> origin .+^ off .+^ ((m * sin (tau * n * t)) *^ norm))
  . map (\t -> (t, tr `atParam` t, tr `normalAtParam` t))
  $ [0, 0.01 .. 1]

test = circle 1 # wiggly 30 0.05 # strokeTrail
