{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module HW5.Evaluator (
    eval
  ) where

import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression, compressWith,
                               decompressWith, defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialise, serialise)
import Control.Monad (foldM, unless, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bitraversable (bimapM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (Foldable (toList))
import Data.Map (Map, assocs, insertWith)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Sequence as Seq
import Data.Set()
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (addUTCTime, diffUTCTime)
import Data.Word (Word8)
import qualified Text.Read as T

import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..))

class HiSlice a where
  toHiValues :: a -> [HiValue]
  fromHiValues :: [HiValue] -> a

instance HiSlice (Seq.Seq HiValue) where
  toHiValues = toList
  fromHiValues = Seq.fromList

instance HiSlice T.Text where
  toHiValues text = HiValueString . T.singleton <$> T.unpack text
  fromHiValues values = T.concat (unpack <$> values)
    where
      unpack :: HiValue -> T.Text
      unpack = \case
        (HiValueString text) -> text
        _                    -> error "Invalid HiValue"

instance HiSlice BS.ByteString where
  toHiValues bytes = HiValueNumber . toRational <$> BS.unpack bytes
  fromHiValues values = BS.pack (unpack <$> values)
    where
      unpack :: HiValue -> Word8
      unpack = \case
        (HiValueNumber num) -> fromIntegral $ numerator num
        _                   -> error "HiValue is not a number"

checkHiFunArity :: HiFun -> Int -> Bool
checkHiFunArity fun argc = case fun of
  HiFunDiv            -> argc == 2
  HiFunMul            -> argc == 2
  HiFunAdd            -> argc == 2
  HiFunSub            -> argc == 2
  HiFunNot            -> argc == 1
  HiFunAnd            -> argc == 2
  HiFunOr             -> argc == 2
  HiFunLessThan       -> argc == 2
  HiFunGreaterThan    -> argc == 2
  HiFunEquals         -> argc == 2
  HiFunNotLessThan    -> argc == 2
  HiFunNotGreaterThan -> argc == 2
  HiFunNotEquals      -> argc == 2
  HiFunIf             -> argc == 3
  HiFunLength         -> argc == 1
  HiFunToUpper        -> argc == 1
  HiFunToLower        -> argc == 1
  HiFunReverse        -> argc == 1
  HiFunTrim           -> argc == 1
  HiFunList           -> True
  HiFunRange          -> argc == 2
  HiFunFold           -> argc == 2
  HiFunPackBytes      -> argc == 1
  HiFunUnpackBytes    -> argc == 1
  HiFunEncodeUtf8     -> argc == 1
  HiFunDecodeUtf8     -> argc == 1
  HiFunZip            -> argc == 1
  HiFunUnzip          -> argc == 1
  HiFunSerialise      -> argc == 1
  HiFunDeserialise    -> argc == 1
  HiFunRead           -> argc == 1
  HiFunWrite          -> argc == 2
  HiFunMkDir          -> argc == 1
  HiFunChDir          -> argc == 1
  HiFunParseTime      -> argc == 1
  HiFunRand           -> argc == 2
  HiFunEcho           -> argc == 1
  HiFunCount          -> argc == 1
  HiFunKeys           -> argc == 1
  HiFunValues         -> argc == 1
  HiFunInvert         -> argc == 1
  HiFunNullary _ -> argc == 0
  HiFunUnary _ -> argc == 1
  HiFunBinary _ -> argc == 2
  HiFunTernary _ -> argc == 3

evalFun :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalFun = \case
  HiFunDiv -> \case
    [HiValueNumber a, HiValueNumber b] -> do
      when (b == 0) $ throwE HiErrorDivideByZero
      return $ HiValueNumber $ a / b
    [HiValueString a, HiValueString b] -> return $ HiValueString $ a <> "/" <> b
    _ -> throwE HiErrorInvalidArgument

  HiFunMul -> \case
    [HiValueNumber a, HiValueNumber b] -> return $ HiValueNumber $ a * b
    [HiValueString a, HiValueNumber b] -> stimesHelper HiValueString b a
    [HiValueList a, HiValueNumber b]   -> stimesHelper HiValueList b a
    [HiValueBytes a, HiValueNumber b]  -> stimesHelper HiValueBytes b a
    _                                  -> throwE HiErrorInvalidArgument
    where
      stimesHelper :: (Semigroup a, HiMonad m)
        => (a -> HiValue)
        -> Rational
        -> a
        -> ExceptT HiError m HiValue
      stimesHelper ctor times a = do
        let t :: Int
            t = truncate times
        unless (t > 0) $ throwE HiErrorInvalidArgument
        return $ ctor $ stimes t a
      

  HiFunAdd -> \case
    [HiValueNumber a, HiValueNumber b] -> return $ HiValueNumber $ a + b
    [HiValueString a, HiValueString b] -> return $ HiValueString $ a <> b
    [HiValueList a, HiValueList b]     -> return $ HiValueList $ a <> b
    [HiValueBytes a, HiValueBytes b]   -> return $ HiValueBytes $ a <> b
    [HiValueTime a, HiValueNumber b]   -> return $ HiValueTime $ addUTCTime (realToFrac b) a
    _                                  -> throwE HiErrorInvalidArgument

  HiFunSub -> \case
    [HiValueNumber a, HiValueNumber b] -> return $ HiValueNumber $ a - b
    [HiValueTime a, HiValueTime b]     -> return $ HiValueNumber $ toRational $ diffUTCTime a b
    _                                  -> throwE HiErrorInvalidArgument

  HiFunNot -> \case
    [HiValueBool a] -> return $ HiValueBool $ not a
    _               -> throwE HiErrorInvalidArgument

  HiFunIf -> \case
    [HiValueBool True, a, _]  -> return a
    [HiValueBool False, _, b] -> return  b
    _                         -> throwE HiErrorInvalidArgument

  HiFunAnd -> \case
    [HiValueNull, _]       -> return HiValueNull
    [HiValueBool False, _] -> return (HiValueBool False)
    [_, b]                 -> return b
    _                      -> throwE HiErrorInvalidArgument

  HiFunOr -> \case
    [HiValueNull, b]       -> return b
    [HiValueBool False, b] -> return b
    [a, _]                 -> return a
    _                      -> throwE HiErrorInvalidArgument

  HiFunLessThan -> \case
    [a, b] -> return $ HiValueBool $ a < b
    _      -> throwE HiErrorInvalidArgument

  HiFunGreaterThan -> \case
    [a, b] -> return $ HiValueBool $ a > b
    _      -> throwE HiErrorInvalidArgument

  HiFunEquals -> \case
    [a, b] -> return $ HiValueBool $ a == b
    _      -> throwE HiErrorInvalidArgument

  HiFunNotLessThan -> \case
    [a, b] -> return $ HiValueBool $ a >= b
    _      -> throwE HiErrorInvalidArgument

  HiFunNotGreaterThan -> \case
    [a, b] -> return $ HiValueBool $ a <= b
    _      -> throwE HiErrorInvalidArgument

  HiFunNotEquals -> \case
    [a, b] -> return $ HiValueBool $ a /= b
    _      -> throwE HiErrorInvalidArgument

  HiFunLength -> \case
    [HiValueString a] -> return $ HiValueNumber $ toRational $ T.length a
    [HiValueList a]   -> return $ HiValueNumber $ toRational $ Seq.length a
    [HiValueBytes a]  -> return $ HiValueNumber $ toRational $ BS.length a
    _                 -> throwE HiErrorInvalidArgument

  HiFunToUpper -> \case
    [HiValueString a] -> return $ HiValueString $ T.toUpper a
    _                 -> throwE HiErrorInvalidArgument

  HiFunToLower -> \case
    [HiValueString a] -> return $ HiValueString $ T.toLower a
    _                 -> throwE HiErrorInvalidArgument

  HiFunReverse -> \case
    [HiValueString a] -> return $ HiValueString $ T.reverse a
    [HiValueList a]   -> return $ HiValueList $ Seq.reverse a
    [HiValueBytes a]  -> return $ HiValueBytes $ BS.reverse a
    _                 -> throwE HiErrorInvalidArgument

  HiFunTrim -> \case
    [HiValueString a] -> return $ HiValueString $ T.strip a
    _                 -> throwE HiErrorInvalidArgument

  HiFunList -> return . HiValueList . Seq.fromList

  HiFunRange -> \case
    [HiValueNumber a, HiValueNumber b] -> return $
      HiValueList $ HiValueNumber <$> [a..b]
    _                                  -> throwE HiErrorInvalidArgument

  HiFunFold -> \case
    [HiValueFunction a, HiValueList b] -> do
      unless (checkHiFunArity a 2) $ throwE HiErrorArityMismatch
      case Seq.viewl b of
        Seq.EmptyL -> return HiValueNull
        start Seq.:< rest -> foldM (\acc cur -> evalFun a [acc, cur]) start rest
    _ -> throwE HiErrorInvalidArgument

  HiFunPackBytes -> \case
    [HiValueList a] -> do
        unless (all isValidByte a) $ throwE HiErrorInvalidArgument
        return $ HiValueBytes $ BS.pack (toList $ unpack <$> a)
    _ -> throwE HiErrorInvalidArgument
    where
        isValidByte :: HiValue -> Bool
        isValidByte = \case
            (HiValueNumber x) -> (denominator x == 1) && 0 <= x && x < 256
            _                 -> False

        unpack :: HiValue -> Word8
        unpack = \case
            (HiValueNumber num) -> fromIntegral $ numerator num
            _                   -> error "HiValue is not a number"

  HiFunUnpackBytes -> \case
    [HiValueBytes a] ->
      let values = HiValueNumber . toRational <$> BS.unpack a
      in return $ HiValueList $ Seq.fromList values
    _ -> throwE HiErrorInvalidArgument

  HiFunEncodeUtf8 -> \case
    [HiValueString a] -> return $ HiValueBytes $ T.encodeUtf8 a
    _                 -> throwE HiErrorInvalidArgument

  HiFunDecodeUtf8 -> \case
    [HiValueBytes a] -> return $
      either (const HiValueNull) HiValueString (T.decodeUtf8' a)
    _                -> throwE HiErrorInvalidArgument

  HiFunZip -> \case
    [HiValueBytes a] ->
      return $ zlibHelper
        (compressWith defaultCompressParams { compressLevel = bestCompression }) a
    _ -> throwE HiErrorInvalidArgument

  HiFunUnzip -> \case
    [HiValueBytes a] ->
      return $ zlibHelper (decompressWith defaultDecompressParams) a
    _ -> throwE HiErrorInvalidArgument

  HiFunSerialise -> \case
    [a] -> return $ HiValueBytes $ BSL.toStrict $ serialise a
    _   -> throwE HiErrorInvalidArgument

  HiFunDeserialise -> \case
    [HiValueBytes a] -> return $ deserialise (BSL.fromStrict a)
    _                -> throwE HiErrorInvalidArgument

  HiFunRead -> \case
    [HiValueString a] -> return $ HiValueAction $ HiActionRead (T.unpack a)
    _                 -> throwE HiErrorInvalidArgument

  HiFunWrite -> \case
    [HiValueString a, HiValueString b] ->
      return $ HiValueAction $ HiActionWrite (T.unpack a) (T.encodeUtf8 b)
    _ -> throwE HiErrorInvalidArgument

  HiFunMkDir -> \case
    [HiValueString a] -> return $ HiValueAction $ HiActionMkDir (T.unpack a)
    _                 -> throwE HiErrorInvalidArgument

  HiFunChDir -> \case
    [HiValueString a] -> return $ HiValueAction $ HiActionChDir (T.unpack a)
    _                 -> throwE HiErrorInvalidArgument

  HiFunParseTime -> \case
    [HiValueString a] -> return $
      maybe HiValueNull HiValueTime (T.readMaybe (T.unpack a))
    _                 -> throwE HiErrorInvalidArgument

  HiFunRand -> \case
    [HiValueNumber a, HiValueNumber b] -> do
      let inBounds rat = fromIntegral (minBound :: Int) <= rat
                          && rat <= fromIntegral (maxBound :: Int)
      unless (inBounds a && inBounds b) $ throwE HiErrorInvalidArgument
      return $ HiValueAction $ HiActionRand (truncate a) (truncate b)
    _ -> throwE HiErrorInvalidArgument

  HiFunEcho -> \case
    [HiValueString a] -> return $ HiValueAction $ HiActionEcho a
    _                 -> throwE HiErrorInvalidArgument
  HiFunKeys -> \case
    [HiValueDict a] -> return $ HiValueList $ Seq.fromList $ Map.keys a
    _               -> throwE HiErrorInvalidArgument

  HiFunValues -> \case
    [HiValueDict a] -> return $ HiValueList $ Seq.fromList $ Map.elems a
    _               -> throwE HiErrorInvalidArgument

  HiFunInvert -> \case
    [HiValueDict a] ->return $ HiValueDict $
      Map.map (HiValueList . Seq.fromList) (foldl invertHelper Map.empty (assocs a))
    _ -> throwE HiErrorInvalidArgument
    where
      invertHelper ::
           Map HiValue [HiValue]
        -> (HiValue, HiValue)
        -> Map HiValue [HiValue]
      invertHelper acc (k, v) = insertWith (++) v [k] acc

  HiFunNullary _ -> \case
    [HiValueNumber arg] -> return $ HiValueNumber (arg - 1)
    [arg]               -> return arg
    _                   -> throwE HiErrorInvalidArgument

  HiFunUnary _ -> \case
    [HiValueNumber arg] -> return $ HiValueNumber (2 * arg)
    [arg]               -> return arg
    _                   -> throwE HiErrorInvalidArgument

  HiFunBinary _ -> \case
    [HiValueNumber arg] -> return $ HiValueNumber (3 * arg)
    [arg1]               -> return arg1
    _                   -> throwE HiErrorInvalidArgument

  HiFunTernary _ -> \case
    [HiValueNumber arg] -> return $ HiValueNumber (4 * arg)
    [arg2]               -> return arg2
    _                   -> throwE HiErrorInvalidArgument

  HiFunCount -> \case
    [HiValueList a]   -> countHelper a
    [HiValueBytes a]  -> countHelper a
    [HiValueString a] -> countHelper a
    _                 -> throwE HiErrorInvalidArgument
    where
      countHelper :: (HiSlice a, HiMonad m) => a -> ExceptT HiError m HiValue
      countHelper slicable =
        let lst = toHiValues slicable
            folder :: Map HiValue Int -> HiValue -> Map HiValue Int
            folder acc cur = insertWith (+) cur 1 acc in
        return $
          HiValueDict $ Map.map
            (HiValueNumber . toRational)
            (foldl folder Map.empty lst)
  where
    zlibHelper :: (BSL.ByteString -> BSL.ByteString) -> BS.ByteString -> HiValue
    zlibHelper action a = HiValueBytes $ BSL.toStrict $ action $ BSL.fromStrict a

-- | Lazy Function evaluator that serves as a guard before usual eval
evalLazyFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalLazyFun fun args = do
  unless (checkHiFunArity fun (length args)) $ throwE HiErrorArityMismatch
  case fun of
    HiFunIf -> case args of
      [a, b, c] -> do
        aEvaled <- eval' a
        case aEvaled of
          HiValueBool True  -> eval' b
          HiValueBool False -> eval' c
          _                 -> throwE HiErrorInvalidArgument
      _ -> throwE HiErrorInvalidArgument
    HiFunAnd -> case args of
      [a, b] -> do
        aEvaled <- eval' a
        case aEvaled of
          HiValueNull       -> return HiValueNull
          HiValueBool False -> return $ HiValueBool False
          _                 -> eval' b
      _ -> throwE HiErrorInvalidArgument
    HiFunOr -> case args of
      [a, b] -> do
        aEvaled <- eval' a
        case aEvaled of
          HiValueNull       -> eval' b
          HiValueBool False -> eval' b
          val               -> return val
      _ -> throwE HiErrorInvalidArgument
    other -> do
      evaledArgs <- mapM eval' args
      evalFun other evaledArgs

-- | Evaluates slicing, works with every HiSlice
evalSlicing :: (HiMonad m, HiSlice a)
  => [HiValue]
  -> a
  -> (a -> HiValue)
  -> ExceptT HiError m HiValue
evalSlicing = \case
  [HiValueNumber a, HiValueNumber b] -> \slicable ctor ->
    let from' = truncate a
        to' = truncate b
        lst = toHiValues slicable
        len = length lst
        from = if from' < 0
          then len + from'
          else from'
        to = if to' < 0
          then len + to'
          else to' in
    return $ ctor $ fromHiValues $ (take (to - from) . drop from) lst
  [a, HiValueNull] -> \slicable -> evalSlicing
    [a, HiValueNumber (toRational $ length $ toHiValues slicable)] slicable
  [HiValueNull, b] -> evalSlicing [HiValueNumber 0, b]
  _ -> \_ _ -> throwE HiErrorInvalidArgument

-- | Evaluates indexing, works with any HiSlice
evalIndexing :: (HiMonad m, HiSlice a)
  => HiValue
  -> a
  -> ExceptT HiError m HiValue
evalIndexing = \case
  (HiValueNumber index) -> \slicable -> do
    let lst = toHiValues slicable
        ind = truncate index
    if ind < 0 || ind >= length lst
      then return HiValueNull
      else return $ lst !! ind
  _ -> \_ -> throwE HiErrorInvalidArgument

-- | Evaluates slice, i.e. either indexing (which is also slicing) or slicing,
boundEvalSlice :: (HiMonad m, HiSlice a)
  => (a -> HiValue)
  -> a
  -> [HiExpr]
  -> ExceptT HiError m HiValue
boundEvalSlice ctor value args = case length args of
  1 -> do
    index <- eval' (head args)
    evalIndexing index value
  2 -> do
    slice <- mapM eval' args
    evalSlicing slice value ctor
  _ -> throwE HiErrorArityMismatch

-- | Evaluates slice
evalSlice :: (HiMonad m) => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalSlice = \case
  (HiValueList a)   -> boundEvalSlice HiValueList a
  (HiValueString a) -> boundEvalSlice HiValueString a
  (HiValueBytes a)  -> boundEvalSlice HiValueBytes a
  _                 -> \_ -> throwE HiErrorInvalidArgument

-- | Evaluates expression in ExceptT context
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' expr = case expr of
  (HiExprValue value) -> return value
  (HiExprApply first other) -> do
    evaled <- eval' first
    case evaled of
      sl@(HiValueString _) -> evalSlice sl other
      sl@(HiValueList _)   -> evalSlice sl other
      sl@(HiValueBytes _)  -> evalSlice sl other
      HiValueDict dict     -> do
        unless (length other == 1) $ throwE HiErrorInvalidArgument
        arg <- eval' (head other)
        return $ fromMaybe HiValueNull (Map.lookup arg dict)
      HiValueFunction fun  -> evalLazyFun fun other
      _                    -> throwE HiErrorInvalidFunction

  (HiExprRun expr') -> do
    evaled <- eval' expr'
    case evaled of
      HiValueAction ha -> lift $ runAction ha
      _                -> throwE HiErrorInvalidArgument

  (HiExprDict vals) -> do
    evaled <- mapM (bimapM eval' eval') vals
    return $ HiValueDict $ Map.fromList evaled


-- | eval evaluates given expression in Hi language.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (eval' expr)