module Color
  ( Color (..)
  , Player
  , otherColor
  ) where


data Color = L | D
  deriving (Eq, Ord, Show, Read, Enum)


type Player = Color


otherColor :: Color -> Color
otherColor L = D
otherColor D = L
