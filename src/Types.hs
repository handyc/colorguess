{-# LANGUAGE GADTs #-}
module Types
  ( Action (..)
  , Result (..)
  , Secret (..)
  , Color (..)
  , GuessPrompt (..)
  , prettyPrint
  , resultPrint
  ) where

data Action = ColorIs Color | Hint | NOOP | End deriving (Read)

data Result = Colder | Correct | Warmer deriving (Enum, Show)

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
prettyPrint Red = "Red"
prettyPrint Orange = "Orange"
prettyPrint Green = "Green"
prettyPrint Yellow = "Yellow"
prettyPrint Blue = "Blue"
prettyPrint Indigo = "Indigo"
prettyPrint Violet = "Violet"
prettyPrint White = "White"
prettyPrint Ultra = "Ultra"

-- | Replacement for Show instance
resultPrint :: Result -> String
resultPrint Colder = "Colder"
resultPrint Correct = "Correct"
resultPrint Warmer = "Warmer"
