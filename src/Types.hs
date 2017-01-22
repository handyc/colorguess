{-# LANGUAGE GADTs #-}
module Types
  ( Action (..)
  , Result (..)
  , Secret (..)
  , Color (..)
  , GuessPrompt (..)
  , prettyPrint
  ) where

data Action = ColorIs Color | Hint | NOOP | End deriving (Read)

data Result = Colder | Correct | Warmer deriving (Enum)

newtype Secret a = Secret { fromSecret :: a }

data Color
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet
  | White
  | Ultra
     deriving (Bounded, Enum, Eq, Ord, Read)

data GuessPrompt a where
  Say   :: String -> GuessPrompt ()
  Query :: String -> GuessPrompt Action
  Guess :: Color -> GuessPrompt Result
  Quit  :: GuessPrompt ()

  -- | Replacement for Show instance
  prettyPrint :: Color -> String
  prettyPrint Red = "the color of anger"
  prettyPrint Orange = "the color of monastic robes"
  prettyPrint Yellow = "the color of cowardice"
  prettyPrint Green = "the color of envy"
  prettyPrint Blue = "the color of sadness"
  prettyPrint Indigo = "the color of my jeans"
  prettyPrint Violet = "like a pretty flower"
  prettyPrint White = "the color of purity"
  prettyPrint Ultra = "the ultimate color, beyond human comprehension"
  prettyPrint _ = "That's not even a color!"
