module Piece
  ( Type (..)
  , Piece (..)
  , belongsTo
  ) where

import Color (Color (..), Player)


data Type = K | Q | R | B | N | P
  deriving (Eq, Ord, Show, Read, Enum)


data Piece
  = None
  | Piece Color Type
  deriving (Eq, Ord, Show)


belongsTo :: Piece -> Player -> Bool
belongsTo (Piece c _) p = c == p
belongsTo _ _           = False
