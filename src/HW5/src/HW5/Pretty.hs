{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
module HW5.Pretty
  ( prettyManyValues
  , prettyValue
  ) where


import Data.ByteString (unpack)
import Data.Foldable (toList)
import Data.Map ()
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import Data.Word (Word8)
import GHC.Exts (IsList)
import HW5.Base (HiAction (..), HiValue (..))
import Prettyprinter (Doc, Pretty (pretty), encloseSep, slash, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

-- | Pretty prints an array of values that are stored in list-like structure
prettyManyValues :: IsList l =>
  (l -> [a])
  -> (a -> Doc AnsiStyle)
  -> Doc AnsiStyle
  -> Doc AnsiStyle
  -> Doc AnsiStyle
  -> l
  -> Doc AnsiStyle
prettyManyValues unpacker formatter left right sep values =
  case values of
    [] -> left <> " " <> right
    xs ->
      let unpacked = unpacker xs
          formatted = map formatter unpacked
      in encloseSep (left <> " ") (" " <> right) sep formatted

-- | Pretty prints arbitrary HiValue
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case
  HiValueFunction fun -> viaShow fun
  HiValueBool True -> "true"
  HiValueBool False -> "false"
  HiValueNull -> "null"
  HiValueString text -> viaShow text
  HiValueList a -> prettyManyValues toList prettyValue "[" "]" ", " a
  (HiValueBytes a) -> prettyManyValues unpack
    (\byte -> pretty (printf "%02x" (byte :: Word8) :: String)) "[#" "#]" " " a
  HiValueDict a -> prettyManyValues Map.toList
    (\(key, value) -> prettyValue key <> ":" <+> prettyValue value) "{" "}" ", " a
  HiValueAction a ->
    case a of
      HiActionRead s -> "read(" <> viaShow s <> ")"
      HiActionWrite s bs -> "write(" <> viaShow s <> "," <+> prettyValue (HiValueBytes bs) <> ")"
      HiActionMkDir s -> "mkdir(" <> viaShow s <> ")"
      HiActionChDir s -> "cd(" <> viaShow s <> ")"
      HiActionCwd -> "cwd"
      HiActionNow -> "now"
      HiActionRand from to -> "rand(" <> pretty from <> "," <+> pretty to <> ")"
      HiActionEcho str -> "echo(" <> viaShow str <> ")"
  HiValueTime time -> "parse-time(\"" <> viaShow time <> "\")"
  HiValueNumber x ->
    let
      num = numerator x
      den = denominator x
    in case fromRationalRepetendUnlimited x of
      (y, Nothing) -> pretty $
        if den == 1
          then show num
          else formatScientific Fixed Nothing y
      (_, Just _) ->
        let
          frac = pretty (rem (abs num) den) <> slash <> pretty den
        in if quot num den /= 0
          then pretty (quot num den) <+> (if x > 0 then "+" else "-") <+> frac
          else (if x > 0 then "" else "-") <> frac