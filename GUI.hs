{-# LANGUAGE NoMonomorphismRestriction #-}

module GUI where

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

type Dia = Diagram B R2

enframe :: Dia -> Dia
enframe d = d # centerXY # sizedAs (r # scale 0.9) <> r
  where
    r :: Dia
    r = rect 2 3

guiFrame :: Dia -> Dia -> Dia
guiFrame d1 d2 = enframe d1 ||| enframe d2

test = guiFrame (circle 1 # lc green) (triangle 2 # lc blue) # frame 0.5
