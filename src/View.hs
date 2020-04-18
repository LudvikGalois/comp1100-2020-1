--- Copyright 2020 The Australian National University, All rights reserved

module View where

import CodeWorld
import Data.Text (pack)
import Model
import Data.Fixed (mod')

-- | Render all the parts of a Model to a CodeWorld picture.
modelToPicture :: Model -> Picture
modelToPicture (Model ss t c)
  = translated 0 8 toolText
  & translated 0 7 colourText
  & colourShapesToPicture ss
  & coordinatePlane
  where
    colourText = stringToText (show c)
    toolText = stringToText (toolToLabel t)
    stringToText = lettering . pack

toolToLabel :: Tool -> String
toolToLabel t = case t of
  LineTool{} -> "Line... click-drag-release"
  PolygonTool{} -> "Polygon... click 3 or more times then spacebar"
  RectangleTool{} -> "Rectangle... click-drag-release"
  CircleTool{} -> "Circle... click-drag-release"
  EllipseTool{} -> "Ellipse... click-drag-release"
  SectorTool{} -> "Sector... click-drag-release"

colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture = mconcat . map colourShapeToPicture

colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (c,s) = colored (colourNameToColour c) (shapeToPicture s)

colourNameToColour :: ColourName -> Colour
colourNameToColour c = case c of
  Black -> black
  Red -> red
  Orange -> orange
  Yellow -> yellow
  Green -> green
  Blue -> blue
  Violet -> purple

(.:) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
(.:) = ((.).(.))

shapeToPicture :: Shape -> Picture
shapeToPicture s = case s of
  Line p q -> polyline [p, q]
  Polygon ps -> solidPolygon ps
  Rectangle p q -> solidPolygon $ zip (map fst [p, p, q, q]) (map snd [p, q, q, p])
  Circle p q -> uncurry translated p $ solidCircle $ dist p q
  Ellipse p q -> translated x y $ scaled w h $ solidCircle 1
    where [(x, w), (y, h)] =
            [ (halfDist + min start end, halfDist)
            | axis <- [fst, snd]
            , let (start, end) = (axis p, axis q)
            , let halfDist = abs (start - end) / 2
            ]
  Sector p q -> uncurry translated p $ sector 0 theta (dist p q)
    where theta = vectorDirection (q `vectorDifference` p) `mod'` (2*pi)

dist :: Point -> Point -> Double
dist = vectorLength .: vectorDifference 
