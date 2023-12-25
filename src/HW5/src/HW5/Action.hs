{-# LANGUAGE LambdaCase #-}

module HW5.Action (
  HiMonad(..),
  HiPermission(..),
  PermissionException(..),
  HIO(..),
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, unless, ap, liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (readFile, writeFile)
import Data.Sequence (fromList)
import Data.Set (Set, member, singleton)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO (putStrLn)
import Data.Time (getCurrentTime)
import HW5.Base (HiAction(..), HiMonad(..), HiValue(..))
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

-- | HiPermission represents a permission given to the Hi program
data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Show, Ord)

-- | PermissionException is thrown when Hi program attempts to perform
newtype PermissionException = PermissionRequired HiPermission
  deriving (Eq)

instance Show PermissionException where
  show (PermissionRequired perm) = "Permission is required: " ++ show perm

instance Exception PermissionException

-- HIO section

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Monad HIO where
  return a = HIO $ \_ -> return a
  HIO hio >>= f = HIO $ \set -> hio set >>= \a -> runHIO (f a) set

instance Applicative HIO where
  pure = return
  (<*>) = ap

instance Functor HIO where
  fmap = liftM

instance MonadIO HIO where
  liftIO io = HIO $ const io

-- | This function performs an IO action in the HIO context
liftIOAction :: Set HiPermission -> (a -> IO b) -> a -> HIO b
liftIOAction perms f a = HIO $ \set -> do
  forM_ perms $ \perm -> unless (member perm set) $ throwIO (PermissionRequired perm)
  f a

-- | Specification of liftIOAction for single AllowRead permission needed
liftReadIO :: (a -> IO b) -> a -> HIO b
liftReadIO = liftIOAction (singleton AllowRead)

-- | Specification of liftIOAction for single AllowWrite permission needed
liftWriteIO :: (a -> IO b) -> a -> HIO b
liftWriteIO = liftIOAction (singleton AllowWrite)

-- | Specification of liftIOAction for single AllowTime permission needed
liftTimeIO :: (a -> IO b) -> a -> HIO b
liftTimeIO = liftIOAction (singleton AllowTime)

instance HiMonad HIO where
  runAction = \case
    -- This action either reads file contents or lists directory
    HiActionRead from -> do
      fileExist <- liftReadIO doesFileExist from
      if fileExist
      then do
        -- Case when provided path exists and is a file
        contents <- liftReadIO Data.ByteString.readFile from
        return (case decodeUtf8' contents of
          Left _ -> HiValueBytes contents
          Right text -> HiValueString text)
      else do
        dirExists <- liftReadIO doesDirectoryExist from
        if dirExists
          then do
            -- Case when provided path exists and is a directory
            list <- liftReadIO listDirectory from
            let wrapped = map (HiValueString . pack) list
            return $ HiValueList (fromList wrapped)
          else
            -- Case when provided path does not exist
            return HiValueNull

      -- This action writes bytes to the destination
    HiActionWrite to bytes -> do
      liftWriteIO (Data.ByteString.writeFile to) bytes
      return HiValueNull

    -- This action creates a directory
    HiActionMkDir filepath -> do
      liftWriteIO createDirectory filepath
      return HiValueNull

    -- This action changes working directory
    HiActionChDir filepath -> do
      liftReadIO setCurrentDirectory filepath
      return HiValueNull

    -- This action returns the current working directory
    HiActionCwd -> do
      current <- liftReadIO (const getCurrentDirectory) ()
      return $ HiValueString (pack current)

    -- This action returns the current time
    HiActionNow -> do
      current <- liftTimeIO (const getCurrentTime) ()
      return $ HiValueTime current

    -- This action returns a random number uniformly distributed on [from ; to]
    HiActionRand from to -> do
      val <- getStdRandom (uniformR (from, to))
      return $ HiValueNumber (fromIntegral val)

    -- This action prints input to stdout
    HiActionEcho str -> do
      liftWriteIO Data.Text.IO.putStrLn str
      return HiValueNull