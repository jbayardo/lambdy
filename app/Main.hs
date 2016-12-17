module Main where

import           Control.Monad      (liftM)
import           Interpreter
import           Parser
import           System.Environment (getArgs)

main :: IO ()
main = do
  filename <- liftM head getArgs
  ast <- parseProgramFromFile filename

  case ast of
    Left error -> print error
    Right ast  -> print $ evaluate ast

