{-# language ScopedTypeVariables #-}

module ParseCmd ( parseCmd ) where

import           Types

import qualified Data.List.Safe as Safe
import           Data.Function ( (&) )
import           Control.Error.Util ( note )

parseCmd :: String -> Either String Command
parseCmd str =
  let cmdTokens = words str
      maybeVerb = Safe.head cmdTokens
      maybeNoun :: Maybe String = ( Safe.!! ) cmdTokens 1
  in do
    verb <- maybeVerb & note "Error: Empty input."
    case verb of
      "add" -> do
        noun <- maybeNoun & note "Error: \"add\" requires a noun."
        return $ Add noun

      "quit" ->
        return Quit

      _ ->
        Left "Error: Unknown command."

