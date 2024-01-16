{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module HW5.Action where

import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import Control.Monad.Extra (ifM)
import Control.Monad.Trans.Reader
import Data.ByteString (writeFile)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, isNothing)
import qualified Data.Sequence as Seq
import Data.Set (Set, member)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import HW5.Base (HiAction (..), HiMonad (runAction), HiValue (HiValueList, HiValueNull, HiValueNumber, HiValueString, HiValueTime))
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random
import Prelude hiding (writeFile)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq)

data PermissionException
  = PermissionRequired HiPermission
  deriving (Show, Ord, Eq)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction (HiActionRead file) =
    runWithPermissions (Just AllowRead) $
      ifM
        (doesDirectoryExist file)
        (HiValueList . Seq.fromList . map (HiValueString . T.pack) <$> listDirectory file)
        $ HiValueString . T.pack <$> readFile file
  runAction (HiActionWrite file text) = runWithPermissions (Just AllowWrite) $ writeFile file text >> pure HiValueNull
  runAction (HiActionChDir dir) = runWithPermissions (Just AllowRead) $ setCurrentDirectory dir >> pure HiValueNull
  runAction (HiActionMkDir dir) = runWithPermissions (Just AllowWrite) $ createDirectory dir >> pure HiValueNull
  runAction HiActionCwd = runWithPermissions (Just AllowRead) $ getCurrentDirectory <&> (HiValueString . T.pack)
  runAction HiActionNow = runWithPermissions (Just AllowTime) $ getCurrentTime <&> HiValueTime
  runAction (HiActionRand l r) = runWithPermissions Nothing $ (randomRIO (l, r) :: IO Int) <&> (HiValueNumber . toRational)
  runAction (HiActionEcho t) = runWithPermissions (Just AllowWrite) $ putStrLn (T.unpack t) >> pure HiValueNull

runWithPermissions :: Maybe HiPermission -> IO HiValue -> HIO HiValue
runWithPermissions permission action =
  HIO $ \permissions ->
    if isNothing permission || member (fromJust permission) permissions then action else throwIO $ PermissionRequired (fromJust permission)
