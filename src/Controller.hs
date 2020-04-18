--- Copyright 2020 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss t c) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> emptyModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show m)) m

      -- display the mystery image
      | k == "M" -> Model mystery t c

      | k == "Backspace" || k == "Delete" -> Model (drop 1 ss) t c
      | k == " " -> case t of
          PolygonTool ps@(_:_:_:_) -> Model ((c, Polygon ps):ss) (PolygonTool []) c
          _ -> m
      | k == "T" -> Model ss (nextTool t) c
      | k == "C" -> Model ss t (nextColour c)

      -- ignore other events
      | otherwise -> m
      where k = unpack key
    PointerPress p -> case t of
      LineTool Nothing -> Model ss (LineTool (Just p)) c
      PolygonTool ps -> Model ss (PolygonTool (p:ps)) c
      RectangleTool Nothing -> Model ss (RectangleTool (Just p)) c
      CircleTool Nothing -> Model ss (CircleTool (Just p)) c
      EllipseTool Nothing -> Model ss (EllipseTool (Just p)) c
      SectorTool Nothing -> Model ss (SectorTool (Just p)) c
      _ -> m
    PointerRelease p -> case t of
      LineTool (Just q) -> Model ((c, Line q p): ss) (LineTool Nothing) c
      RectangleTool (Just q) -> Model ((c, Rectangle q p): ss) (RectangleTool Nothing) c
      CircleTool (Just q) -> Model ((c, Circle q p): ss) (CircleTool Nothing) c
      EllipseTool (Just q) -> Model ((c, Ellipse q p): ss) (EllipseTool Nothing) c
      SectorTool (Just q) -> Model ((c, Sector q p): ss) (SectorTool Nothing) c
      _ -> m
    _ -> m

nextColour :: ColourName -> ColourName
nextColour c = case c of
  Black -> Red
  Red -> Orange
  Orange -> Yellow
  Yellow -> Green
  Green -> Blue
  Blue -> Violet
  Violet -> Black

nextTool :: Tool -> Tool
nextTool t = case t of
  LineTool Nothing -> PolygonTool []
  PolygonTool [] -> RectangleTool Nothing
  RectangleTool Nothing -> CircleTool Nothing
  CircleTool Nothing -> EllipseTool Nothing
  EllipseTool Nothing -> SectorTool Nothing
  SectorTool Nothing -> LineTool Nothing
  _ -> t

