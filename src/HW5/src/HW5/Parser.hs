module HW5.Parser (parse) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString (pack)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Text (pack)
import Data.Word()
import Data.Void (Void)
import HW5.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec (MonadParsec (eof, label, notFollowedBy, try), ParseErrorBundle, Parsec, between, choice, empty, many, manyTill, runParser, satisfy, sepBy, sepBy1, sepEndBy, (<|>))
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L


-- General helpers section
type Parser = Parsec Void String

skipSpaces :: Parser ()
skipSpaces = L.space space1 empty empty

-- | Parses lexeme and skips spaces afterwards
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpaces

-- | Parses symbol and skips spaces afterwards
symbol :: String -> Parser String
symbol = L.symbol skipSpaces

-- | General bracket parser
brackets :: String -> String -> Parser a -> Parser a
brackets open close = between (symbol open) (symbol close)

-- | Transform parser to parse something between round brackets
parenthesis :: Parser a -> Parser a
parenthesis = brackets "(" ")"

-- | Parser of something that is prefix of another something with lower priority
op :: String -> String -> Parser String
op n next = (lexeme . try) (string n <* notFollowedBy (symbol next))

-- | Ultimate parser of infix binary expressions
binary' :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> Parser String -> HiFun -> Operator Parser HiExpr
binary' ctor p f = ctor (wrapper <$ p)
  where
    wrapper :: HiExpr -> HiExpr -> HiExpr
    wrapper a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

binaryL, binaryN, binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryL name = binary' InfixL (symbol name)
binaryN name = binary' InfixN (symbol name)
binaryR name = binary' InfixR (symbol name)

-- HiValue section

pHiValueBool, pHiValueFunction, pHiValueNumber, pHiValueNull, pHiValueString, pHiValueBytes, pHiValueAction :: Parser HiValue
pHiValueBool = label "boolean" $ HiValueBool <$> choice [True <$ string "true", False <$ string "false"]
pHiValueFunction = label "Hi function" $ HiValueFunction <$> choice hiFunParsers
  where
    hiFunParsers =
        [ HiFunNotLessThan    <$ string "not-less-than"
        , HiFunNotGreaterThan <$ string "not-greater-than"
        , HiFunNotEquals      <$ string "not-equals"
        , HiFunDiv            <$ string "div"
        , HiFunMul            <$ string "mul"
        , HiFunAdd            <$ string "add"
        , HiFunSub            <$ string "sub"
        , HiFunNot            <$ string "not"
        , HiFunAnd            <$ string "and"
        , HiFunOr             <$ string "or"
        , HiFunLessThan       <$ string "less-than"
        , HiFunGreaterThan    <$ string "greater-than"
        , HiFunEquals         <$ string "equals"
        , HiFunIf             <$ string "if"
        , HiFunLength         <$ string "length"
        , HiFunToUpper        <$ string "to-upper"
        , HiFunToLower        <$ string "to-lower"
        , HiFunReverse        <$ string "reverse"
        , HiFunTrim           <$ string "trim"
        , HiFunList           <$ string "list"
        , HiFunRange          <$ string "range"
        , HiFunFold           <$ string "fold"
        , HiFunPackBytes      <$ string "pack-bytes"
        , HiFunUnpackBytes    <$ string "unpack-bytes"
        , HiFunEncodeUtf8     <$ string "encode-utf8"
        , HiFunDecodeUtf8     <$ string "decode-utf8"
        , HiFunZip            <$ string "zip"
        , HiFunUnzip          <$ string "unzip"
        , HiFunSerialise      <$ string "serialise"
        , HiFunDeserialise    <$ string "deserialise"
        , HiFunRead           <$ string "read"
        , HiFunWrite          <$ string "write"
        , HiFunMkDir          <$ string "mkdir"
        , HiFunChDir          <$ string "cd"
        , HiFunParseTime      <$ string "parse-time"
        , HiFunRand           <$ string "rand"
        , HiFunEcho           <$ string "echo"
        , HiFunCount          <$ string "count"
        , HiFunKeys           <$ string "keys"
        , HiFunValues         <$ string "values"
        , HiFunInvert         <$ string "invert"
        ]

-- | Parses number into HiValueNumber
pHiValueNumber = label "number" $ HiValueNumber . toRational <$> L.signed skipSpaces L.scientific

-- | Parses null value into HiValueNull
pHiValueNull = label "null" $ HiValueNull <$ string "null"

-- | Parses string literal into HiValueString
pHiValueString = label "string literal" $ HiValueString . Data.Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

-- | Parses byte array into HiValueBytes
pHiValueBytes = label "bytes" $ HiValueBytes . Data.ByteString.pack <$> brackets "[#" "#]" (pWord8 `sepEndBy` space1)
  where
    pWord8 = label "byte" $ read . ("0x" ++) <$> sequenceA [hexDigitChar, hexDigitChar]

-- | Parses pure (0-arity) actions into HiValueAction
pHiValueAction = label "action" $ HiValueAction <$> choice [HiActionCwd <$ string "cwd", HiActionNow <$ string "now"]

-- | Parses arbitrary HiValue
pHiValue :: Parser HiValue
pHiValue = label "hi value" $ choice [pHiValueBool, pHiValueFunction, pHiValueNumber, pHiValueNull, pHiValueString, pHiValueBytes, pHiValueAction]

-- | Parses list of values into list (aka application of HiFunList to all)
pList :: Parser HiExpr
pList = label "list" $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> brackets "[" "]" (pInfix `sepBy` symbol ",")

-- | Parses hi value as expression
pHiExprValue :: Parser HiExpr
pHiExprValue = label "hi expr value" $ parenthesis pHiExprValue <|> HiExprValue <$> lexeme pHiValue <|> pList <|> pDictionary

-- | Parses dictionary item (aka pair)
pDictionaryItem :: Parser (HiExpr, HiExpr)
pDictionaryItem = label "dictionary item" $ (,) <$> (pInfix <* symbol ":") <*> pInfix

-- | Parses dictionary
pDictionary :: Parser HiExpr
pDictionary = label "dictionary" $ HiExprDict <$> brackets "{" "}" (pDictionaryItem `sepBy` symbol ",")

-- | Parses run symbol, aka '!'
pAction :: Parser (HiExpr -> HiExpr)
pAction = label "hi expr run" $
  HiExprRun <$ symbol "!"

pDotAccess :: Parser (HiExpr -> HiExpr)
pDotAccess = lexeme $ do
  _ <- lexeme $ char '.'
  text <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return $ \expr ->
    HiExprApply expr [HiExprValue $ HiValueString (Data.Text.pack $ intercalate "-" text)]

-- | Parses usual arglist
pArguments :: Parser (HiExpr -> HiExpr)
pArguments = flip HiExprApply <$> parenthesis (pInfix `sepBy` symbol ",")

pHiExpr :: Parser HiExpr
pHiExpr = label "hi expr" $ pHiExprInner <|> parenthesis pHiExpr
  where
    pHiExprInner = do
      main <- pHiExprValue
      rest <- many (pArguments <|> pAction <|> pDotAccess)
      return $ foldl (\acc cur -> cur acc) main rest

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [binaryL "*" HiFunMul, binary' InfixL (op "/" "=") HiFunDiv]
  , [binaryL "+" HiFunAdd, binaryL "-" HiFunSub]
  , [binaryN "<=" HiFunNotGreaterThan, binaryN ">=" HiFunNotLessThan, binaryN "<" HiFunLessThan, binaryN ">" HiFunGreaterThan, binaryN "==" HiFunEquals, binaryN "/=" HiFunNotEquals]
  , [binaryR "&&" HiFunAnd]
  , [binaryR "||" HiFunOr]
  ]

pInfix :: Parser HiExpr
pInfix = makeExprParser (try pHiExpr <|> parenthesis pInfix) operatorTable

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between skipSpaces eof pInfix) ""