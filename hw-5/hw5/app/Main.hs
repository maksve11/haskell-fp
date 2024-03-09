{-# LANGUAGE TypeApplications #-}
module Main (
  main
) where

import Control.Exception (catch)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Monoid ()
import Data.Set (Set, fromList)
import Data.Void (Void)
import HW5.Action (HIO (runHIO), HiPermission (..), PermissionException)
import HW5.Base (HiError, HiValue)
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Console.Haskeline (InputT, defaultSettings, getExternalPrint, getInputLine,
                                 runInputT)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

-- | All the permissions for REPL
fullPermissions :: Set HiPermission
fullPermissions = fromList [
     AllowRead
  ,  AllowWrite
  ,  AllowTime
  ]

-- | Parses and possibly evaluates given string with given set of permissions
processLine ::
     Set HiPermission
  -> String
  -> IO (Either (ParseErrorBundle String Void) (Either HiError HiValue))
processLine permissions str = do
  let parsed = parse str
  mapM (\expr -> runHIO (eval expr) permissions) parsed

-- | Pretty-prints result of line processing
showResult ::
     IO (Either (ParseErrorBundle String Void) (Either HiError HiValue))
  -> IO String
showResult parsed = do
  result <- parsed
  case result of
    Left errorBundle -> return $ errorBundlePretty errorBundle
    Right expression -> case expression of
      Left evalError -> return $ show evalError
      Right value -> return $ renderString $ layoutPretty defaultLayoutOptions (prettyValue value)

-- | Starts repl with specified prompt and input handler
repl :: String -> ((String -> IO ()) -> String -> IO ()) -> InputT IO()
repl prompt handler = replLoop
  where
    replLoop = do
      input <- getInputLine prompt
      case input of
        Nothing -> return ()
        Just userInput -> do
          printer <- getExternalPrint
          lift $ handler printer userInput
          replLoop

main :: IO ()
main = runInputT defaultSettings $
  repl "hi>" handler
    where
      handler :: (String -> IO ()) -> String -> IO ()
      handler printer input =
        catch @PermissionException
          ((showResult . processLine fullPermissions) input >>= printer)
          print