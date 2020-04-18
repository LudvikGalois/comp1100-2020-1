--- Copyright 2020 The Australian National University, All rights reserved

module View where

import CodeWorld
import Data.Text (pack)
import Model
import Data.Fixed (mod')

-- | Render all the parts of a Model to a CodeWorld picture.
modelToPicture :: Model -> Picture
modelToPicture (Model ss t c pLoc)
  = translated 0 8 toolText
  & translated 0 7 colourText
  & (preview t c pLoc)
  & colourShapesToPicture ss
  & coordinatePlane
  where
    colourText = stringToText (show c)
    toolText = stringToText (toolToLabel t)
    stringToText = lettering . pack

preview :: Tool -> ColourName -> Maybe Point -> Picture
preview _ _ Nothing = mempty
preview t c (Just p) = case shape of
  Nothing -> mempty
  Just s -> colored (translucent (colourNameToColour c)) (shapeToPicture s)
  where
    shape = case t of
      LineTool (Just q) -> Just $ Line q p
      PolygonTool [q] -> Just $ Line q p
      PolygonTool ps@(_:_:_) -> Just $ Polygon (p:ps)
      RectangleTool (Just q) -> Just $ Rectangle q p
      CircleTool (Just q) -> Just $ Circle q p
      EllipseTool (Just q) -> Just $ Ellipse q p
      SectorTool (Just q) -> Just $ Sector q p
      PenTool True ps -> Just $ Pen ps
      _ -> Nothing

toolToLabel :: Tool -> String
toolToLabel t = case t of
  LineTool{} -> "Line... click-drag-release"
  PolygonTool{} -> "Polygon... click 3 or more times then spacebar"
  RectangleTool{} -> "Rectangle... click-drag-release"
  CircleTool{} -> "Circle... click-drag-release"
  EllipseTool{} -> "Ellipse... click-drag-release"
  SectorTool{} -> "Sector... click-drag-release"
  PenTool{} -> "Pen... click-drag-release"

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

shapeToPicture :: Shape -> Picture
shapeToPicture s = case s of
  Line p q -> polyline [p, q]
  Polygon ps -> solidPolygon ps
  Rectangle p q -> solidPolygon $ zip (map fst [p, p, q, q]) (map snd [p, q, q, p])
  Circle p@(x,y) q -> translated x y $ solidCircle $ dist p q
  Ellipse p q -> translated x y $ scaled halfWidth halfHeight $ solidCircle 1
    where [(x, halfWidth), (y, halfHeight)] =
            [ (halfDist + min start end, halfDist)
            | axis <- [fst, snd]
            , let (start, end) = (axis p, axis q)
            , let halfDist = abs (start - end) / 2
            ]
  Sector p@(x,y) q -> translated x y $ sector 0 theta (dist p q)
    where theta = vectorDirection (q `vectorDifference` p) `mod'` (2*pi)
  Pen ps -> curve ps

dist :: Point -> Point -> Double
dist p q = vectorLength (vectorDifference p q)
