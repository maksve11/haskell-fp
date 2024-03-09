{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW4.Parser
  ( Parser(..)
  , ParseError(..)
  , runP
  , ok
  ) where

import Control.Applicative
import Control.Monad
import GHC.Natural (Natural)
import HW4.T1(ExceptState (..))
import HW4.Types

data ParseError =
  ErrorAtPos Natural
  deriving (Show, Eq)

newtype Parser a =
  P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P eSt) str =
  case runES eSt (0, str) of
    Error err               -> Error err
    (Success (parsed :# _)) -> Success parsed

parseError :: Parser a
parseError = P (ES (\(pos, _) -> Error (ErrorAtPos pos)))

instance Alternative Parser where
  empty = parseError
  (<|>) (P frst) (P scnd) =
    P
      (ES
         (\state ->
            case runES frst state of
              Error _ -> runES scnd state
              success -> success))

instance MonadPlus Parser

ok :: Parser ()
ok = P (ES (\(_, _) -> Success (() :# (0, ""))))