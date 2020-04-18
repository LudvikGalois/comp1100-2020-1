--- Copyright 2020 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss t c pLoc) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> emptyModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show m)) m

      -- display the mystery image
      | k == "M" -> Model mystery t c pLoc

      | k == "Backspace" || k == "Delete" -> Model (drop 1 ss) t c pLoc
      | k == " " -> case t of
          PolygonTool ps@(_:_:_:_) -> Model ((c, Polygon ps):ss) (PolygonTool []) c pLoc
          _ -> m
      | k == "T" -> Model ss (nextTool t) c pLoc
      | k == "C" -> Model ss t (nextColour c) pLoc

      -- ignore other events
      | otherwise -> m
      where k = unpack key
    PointerPress p -> case t of
      LineTool Nothing -> Model ss (LineTool (Just p)) c pLoc
      PolygonTool ps -> Model ss (PolygonTool (p:ps)) c pLoc
      RectangleTool Nothing -> Model ss (RectangleTool (Just p)) c pLoc
      CircleTool Nothing -> Model ss (CircleTool (Just p)) c pLoc
      EllipseTool Nothing -> Model ss (EllipseTool (Just p)) c pLoc
      SectorTool Nothing -> Model ss (SectorTool (Just p)) c pLoc
      PenTool False [] -> Model ss (PenTool True []) c pLoc
      _ -> m
    PointerRelease p -> case t of
      LineTool (Just q) -> Model ((c, Line q p): ss) (LineTool Nothing) c pLoc
      RectangleTool (Just q) -> Model ((c, Rectangle q p): ss) (RectangleTool Nothing) c pLoc
      CircleTool (Just q) -> Model ((c, Circle q p): ss) (CircleTool Nothing) c pLoc
      EllipseTool (Just q) -> Model ((c, Ellipse q p): ss) (EllipseTool Nothing) c pLoc
      SectorTool (Just q) -> Model ((c, Sector q p): ss) (SectorTool Nothing) c pLoc
      PenTool True ps -> Model ((c, Pen ps): ss) (PenTool False []) c pLoc
      _ -> m
    PointerMovement p
      | (PenTool True ps) <- t -> Model ss (PenTool True (p:ps)) c (Just p)
      | otherwise -> Model ss t c (Just p)
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
  SectorTool Nothing -> PenTool False []
  PenTool False [] -> LineTool Nothing
  _ -> t

