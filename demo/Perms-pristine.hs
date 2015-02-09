{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.List
import Data.List.Split
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

type Dia = Diagram B

colors = [red, green, blue, orange]


main = defaultMain (circle 1 # fc blue # frame 0.5)
