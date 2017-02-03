module Main where

import Prelude
--import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console --(CONSOLE, log)
import Math (sqrt, pi)

diagonal w h = sqrt (w * w + h * h)
circleArea r = pi * r * r

main = logShow (circleArea 3.0) 
