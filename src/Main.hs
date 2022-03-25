{-# language GADTs            #-}
{-# language FlexibleContexts #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = 
  mainWith d >> putStrLn "Done!"


d :: [(Diagram B, Int)]
d = zip fullAnimation (repeat 10)
  where
    speed = 10

    scrunchFrames        = map scrunchUp [20 .. 40]
    slideUpFrames        = map (slideUp $ last scrunchFrames) [0 .. 30]
    slideAcrossFrames    = map (slideAcross $ last slideUpFrames) [1 .. 30]
    settleInCenterFrames = map (settleInCenter 30 $ last slideAcrossFrames) [0 .. 30]
    placeDownFrames      = map (placeDown 30 $ last settleInCenterFrames) [0 .. 30]
    placeFlatFrames      = map (placeFlat 15 $ last placeDownFrames) [0 .. 15]

    fullAnimation = 
           map mkFrame scrunchFrames
        ++ map mkFrame slideUpFrames
        ++ map mkFrame slideAcrossFrames
        ++ map mkFrame settleInCenterFrames
        ++ map mkFrame placeDownFrames
        ++ map mkFrame placeFlatFrames


type Bezier = (V2 Double, V2 Double, V2 Double) 


placeFlat :: Integer -> Bezier -> Integer -> Bezier
placeFlat m (c1', c2', x2') i = (c1, c2, x2)
  where
    j = fromInteger i
    l = fromInteger m
    r = 1 - (j / l)
    r' = j / l
    c1 = c1' -- Already at 0.
    c2 = (c2' * r2 (r, r) + r2 (1-r, 1-r) * r2 (0,0.5))
    x2 = x2' * r2 (1 - r', 1 - r') + r2 (r', r') * r2 (-2.8,0)

placeDown :: Integer -> Bezier -> Integer -> Bezier
placeDown m (c1', c2', x2') i = (c1, c2, x2)
  where
    j = fromInteger i
    l = fromInteger m
    r = 1 - (j / l)
    r' = j / l
    c1 = c1' -- Already at 0.
    c2 = c2' - (c2' * r2 (r, r) + r2 (1-r, 1-r) * r2 (1,0.5))
    x2 = x2' * r2 (1 - r', 1 - r') + r2 (r', r') * r2 (-2.5,1)

-- | 
settleInCenter :: Integer -> Bezier -> Integer -> Bezier
settleInCenter m (c1', c2', x2') i = (c1, c2, x2)
  where
    j = fromInteger i
    l = fromInteger m
    r = j / l
    c1 = c1' - ( c1' * r2 (r, r) + r2 (1 - r, 1 - r) * r2 (0,0) )
    c2 = c2' - ( c2' * r2 (r, r) + r2 (1 - r, 1 - r) * r2 (0,0) )
    x2 = x2' + r2 (0 - j * 0.04, j * 0.03)

-- | Slide across a bit.
slideAcross :: Bezier -> Integer -> Bezier
slideAcross (c1', c2', x2') i = (c1, c2, x2)
  where
    j = fromInteger i
    c1 = c1' + r2 (j * 0.05, 0)
    c2 = c2' + r2 (0 - j * 0.03, 0)
    x2 = x2' + r2 (0 - j * 0.04, 0)

-- | Control point 2 comes left, x2 comes bup.
slideUp :: Bezier -> Integer -> Bezier
slideUp (c1', c2', x2') i = (c1, c2, x2)
  where
    j = fromInteger i
    c1 = c1' + r2 (0, 0 - j * 0.005)
    c2 = c2' -- + r2 (0 - j * 0.03, 0)
    x2 = x2' + r2 (0 - j * 0.01, j * 0.04)

scrunchUp :: Integer -> Bezier
scrunchUp i = (c1, c2, x2)
  where
    j = fromInteger i
    [c1, c2, x2] = map r2 [(1, 0), (2 + j *0.02, j*0.02), (3 - j*0.01, 0)]

initialFold :: Integer -> Bezier
initialFold i = (c1, c2, x2)
  where
    j = fromInteger i
    [c1, c2, x2] = map r2 [(1, 0), (2 + j *0.02, j*0.02), (3 - j*0.01, 0)]

mkFrame :: Bezier -> Diagram B
mkFrame (c1, c2, x2) = 
  ( bez
  <> square 10
  <> hrule 5.8 # translate (r2 (0, -0.25))
  <> b (initialFold 20 # reflectX)  # (translate (r2 (0, -0.1)))
  <> b (initialFold 20)  # (translate (r2 (0, -0.1)))
  )
  # bg white
  # lw 4
  where
    b (c1, c2, x2) = illustrateBezier c1 c2 x2
    bez = illustrateBezier c1 c2 x2


illustrateBezier c1 c2 x2
    =  mempty
    -- <> endpt
    -- <> endpt  # translate x2
    -- <> ctrlpt # translate c1
    -- <> ctrlpt # translate c2
    -- <> l1
    -- <> l2
    <> fromSegments [bezier3 c1 c2 x2]
  where
    -- endpt  = circle 0.05 # fc red  # lw none
    -- dashed = dashingN [0.03,0.03] 0
    -- ctrlpt = circle 0.05 # fc blue # lw none
    -- l1     = fromOffsets [c1] # dashed
    -- l2     = fromOffsets [x2 ^-^ c2] # translate c2 # dashed

