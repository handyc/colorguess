module Utilities
  ( pickColor, colors, spacedPrint ) where

import System.Random (RandomGen)
import System.Random.Shuffle (shuffle')

import Types (Secret (..), Color (..), prettyPrint)
import Data.Monoid ((<>))

-- | Utilities for color choosing
pickColor :: RandomGen a => a -> Secret Color
pickColor gen = Secret color
  where (color: _) = shuffle' colors (length colors) gen

colors :: [Color]
colors = [minBound .. maxBound]

spacedPrint :: Color -> String
spacedPrint color = prettyPrint color <> formatting
  where
    formatting = if (color == maxBound) then "" else " "
