module ColorGuess (runGameM_) where


import Control.Monad.State (StateT,runStateT)
import System.Random (newStdGen)
import Control.Monad.Prompt (Prompt,runPromptM,prompt)
import Data.Monoid ((<>))

import Types
import Utilities
import Interpreter

runGameM_ :: IO ()
runGameM_ = runGameM >> return ()
  where runGameM = runStateT game =<< pickColor <$> newStdGen

game ::StateT (Secret Color) IO ()
game = (runPromptM gameInterp play)

play :: Prompt GuessPrompt ()
play = do
  action <- queryPrompt
  case action of
    (ColorIs color) -> guess color
    Hint          -> hint
    NOOP          -> play
    End           -> end

queryPrompt :: Prompt GuessPrompt Action
queryPrompt = prompt (Say commands) >> prompt (Query guess')
  where
    guess'    = "Make a guess. Colors are " <> (concatMap spacedPrint colors)
    commands = "Commands are <ColorIs color> <Hint> <End>"

guess :: Color -> Prompt GuessPrompt ()
guess color = prompt (Guess color) >>= fromResult

hint :: Prompt GuessPrompt ()
hint = prompt (Say "a hint, really?") >> play

end :: Prompt GuessPrompt ()
end = prompt Quit

fromResult :: Result -> Prompt GuessPrompt ()
fromResult Correct = prompt (Say "you are correct sir!") >> end
fromResult res     = prompt (Say (resultPrint res)) >> play
