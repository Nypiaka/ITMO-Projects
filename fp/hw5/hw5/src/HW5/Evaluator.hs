{-# LANGUAGE LambdaCase #-}

module HW5.Evaluator
  ( eval,
  )
where

import Codec.Compression.Zlib
import Codec.Serialise (deserialise, serialise)
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.ByteString (drop, index, length, pack, take, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import qualified Data.Map
import Data.Semigroup
import Data.Sequence (fromList)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.Lazy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock (addUTCTime)
import HW5.Base
import Text.Read (readMaybe)
import Prelude hiding (drop, length, reverse, take)

isUnFunc :: HiFun -> Bool
isUnFunc f =
  Set.member f $
    Set.fromList
      [ HiFunNot,
        HiFunLength,
        HiFunToUpper,
        HiFunToLower,
        HiFunReverse,
        HiFunTrim,
        HiFunPackBytes,
        HiFunUnpackBytes,
        HiFunEncodeUtf8,
        HiFunDecodeUtf8,
        HiFunZip,
        HiFunUnzip,
        HiFunSerialise,
        HiFunDeserialise,
        HiFunRead,
        HiFunMkDir,
        HiFunChDir,
        HiFunParseTime,
        HiFunEcho,
        HiFunCount
      ]

isBiFunc :: HiFun -> Bool
isBiFunc f =
  Set.member f $
    Set.fromList
      [ HiFunAdd,
        HiFunDiv,
        HiFunMul,
        HiFunSub,
        HiFunEquals,
        HiFunGreaterThan,
        HiFunLessThan,
        HiFunNotEquals,
        HiFunNotGreaterThan,
        HiFunNotLessThan,
        HiFunRange,
        HiFunFold,
        HiFunAnd,
        HiFunOr,
        HiFunWrite,
        HiFunRand,
        HiFunKeys,
        HiFunValues,
        HiFunInvert
      ]

evalUnFuncTemplate :: HiMonad m => HiExpr -> (HiValue -> m (Either HiError HiValue)) -> m (Either HiError HiValue)
evalUnFuncTemplate a transformer = runExceptT $ do
  valA <- ExceptT $ eval a
  ExceptT $ transformer valA

invert :: Map HiValue HiValue -> HiValue
invert m =
  HiValueDict $
    Data.Map.foldrWithKey
      ( \k v pm ->
          case Data.Map.lookup v pm of
            Just smt -> case smt of
              (HiValueList lsmt) -> Data.Map.insert v (HiValueList (lsmt Seq.|> k)) pm
              va -> Data.Map.insert v (HiValueList (Seq.fromList [va, k])) pm
            Nothing -> Data.Map.insert v (HiValueList $ Seq.singleton k) pm
      )
      Data.Map.empty
      m

countMapTemplate :: Foldable t => t l -> (l -> HiValue) -> HiValue
countMapTemplate list transformer =
  HiValueDict $
    Prelude.foldl
      ( \pm c ->
          Data.Map.insertWith
            ( \a b -> HiValueNumber $ case (a, b) of
                (HiValueNumber na, HiValueNumber nb) -> na + nb
                _ -> 0
            )
            (transformer c)
            (HiValueNumber 1)
            pm
      )
      Data.Map.empty
      list

getIndTemplate :: HiMonad m => HiExpr -> Int -> (Int -> HiValue) -> m (Either HiError HiValue)
getIndTemplate expr l transformer = evalUnFuncTemplate expr $ \i -> runExceptT $ case i of
  HiValueNumber n ->
    if fromEnum n < 0 || fromEnum n >= l
      then return HiValueNull
      else return $ transformer $ fromEnum n
  _ -> throwE HiErrorArityMismatch

unFuncMap :: HiMonad m => HiValue -> HiExpr -> m (Either HiError HiValue)
unFuncMap (HiValueFunction fun) expr =
  evalUnFuncTemplate expr $ \val -> runExceptT $
    case (fun, val) of
      (HiFunNot, HiValueBool b) -> return $ HiValueBool (not b)
      (HiFunLength, HiValueString s) -> return $ HiValueNumber $ toRational (T.length s)
      (HiFunLength, HiValueList l) -> return $ HiValueNumber (toRational (Seq.length l))
      (HiFunToUpper, HiValueString s) -> return $ HiValueString $ toUpper s
      (HiFunToLower, HiValueString s) -> return $ HiValueString $ toLower s
      (HiFunReverse, HiValueString s) -> return $ HiValueString $ T.reverse s
      (HiFunReverse, HiValueList l) -> return $ HiValueList $ Seq.reverse l
      (HiFunTrim, HiValueString s) -> return $ HiValueString $ strip s
      (HiFunPackBytes, HiValueList l) -> do
        bytes <- mapM (\case HiValueNumber n -> return $ fromIntegral (fromEnum n); _ -> throwE HiErrorInvalidArgument) l
        return $ HiValueBytes $ Data.ByteString.pack $ toList bytes
      (HiFunUnpackBytes, HiValueBytes b) ->
        return $
          HiValueList $
            Seq.fromList $
              Prelude.map (HiValueNumber . toRational) (Data.ByteString.unpack b)
      (HiFunEncodeUtf8, HiValueString s) -> return $ HiValueBytes $ toStrict $ encodeUtf8 (Data.Text.Lazy.fromStrict s)
      (HiFunDecodeUtf8, HiValueBytes b) -> case decodeUtf8' b of
        Left _ -> return HiValueNull
        Right text -> return $ HiValueString text
      (HiFunZip, HiValueBytes b) ->
        return $ HiValueBytes $ toStrict $ compressWith (defaultCompressParams {compressLevel = bestCompression}) (fromStrict b)
      (HiFunUnzip, HiValueBytes b) -> return $ HiValueBytes $ toStrict (decompressWith defaultDecompressParams (fromStrict b))
      (HiFunSerialise, value) -> return $ HiValueBytes $ toStrict $ serialise value
      (HiFunDeserialise, HiValueBytes b) -> return $ deserialise $ fromStrict b
      (HiFunRead, HiValueString s) -> return $ HiValueAction $ HiActionRead $ T.unpack s
      (HiFunMkDir, HiValueString s) -> return $ HiValueAction $ HiActionMkDir $ T.unpack s
      (HiFunChDir, HiValueString s) -> return $ HiValueAction $ HiActionChDir $ T.unpack s
      (HiFunParseTime, HiValueString s) -> return $ maybe HiValueNull HiValueTime (readMaybe (T.unpack s) :: Maybe UTCTime)
      (HiFunEcho, HiValueString s) -> return $ HiValueAction $ HiActionEcho s
      (HiFunKeys, HiValueDict m) -> return $ HiValueList $ Seq.fromList $ Data.Map.keys m
      (HiFunValues, HiValueDict m) -> return $ HiValueList $ Seq.fromList $ Data.Map.elems m
      (HiFunInvert, HiValueDict m) -> return $ invert m
      (HiFunCount, HiValueString s) -> return $ countMapTemplate (T.unpack s) (\c -> HiValueString (T.pack [c]))
      (HiFunCount, HiValueList l) -> return $ countMapTemplate l id
      (HiFunCount, HiValueBytes by) -> return $ countMapTemplate (Data.ByteString.unpack by) $ HiValueNumber . toRational
      _ -> if isUnFunc fun then throwE HiErrorInvalidArgument else throwE HiErrorArityMismatch
unFuncMap (HiValueString str) expr = getIndTemplate expr (T.length str) (\num -> HiValueString $ T.drop num $ T.take (num + 1) str)
unFuncMap (HiValueList l) expr = getIndTemplate expr (Seq.length l) (Seq.index l)
unFuncMap (HiValueBytes b) expr = getIndTemplate expr (Data.ByteString.length b) (HiValueNumber . toRational . Data.ByteString.index b)
unFuncMap (HiValueDict dict) expr = runExceptT $ do
  evaled <- ExceptT $ eval expr
  return $ Data.Map.findWithDefault HiValueNull evaled dict
unFuncMap _ _ = return $ Left HiErrorInvalidFunction

evalBiFuncTemplate :: HiMonad m => HiExpr -> HiExpr -> (HiValue -> HiValue -> m (Either HiError HiValue)) -> m (Either HiError HiValue)
evalBiFuncTemplate a b transformer = runExceptT $ do
  valA <- ExceptT $ eval a
  valB <- ExceptT $ eval b
  ExceptT $ transformer valA valB

biFuncMap :: HiMonad m => HiValue -> HiExpr -> HiExpr -> m (Either HiError HiValue)
biFuncMap (HiValueFunction HiFunAnd) exprA exprB =
  runExceptT $ do
    evaledA <- ExceptT $ eval exprA
    case evaledA of
      HiValueBool False -> return $ HiValueBool False
      HiValueNull -> return HiValueNull
      _ -> ExceptT $ eval exprB
biFuncMap (HiValueFunction HiFunOr) exprA exprB =
  runExceptT $ do
    evaledA <- ExceptT $ eval exprA
    case evaledA of
      HiValueBool False -> ExceptT $ eval exprB
      HiValueNull -> ExceptT $ eval exprB
      _ -> return evaledA
biFuncMap (HiValueFunction fun) exprA exprB =
  evalBiFuncTemplate exprA exprB $ \valA valB -> runExceptT $
    case (fun, valA, valB) of
      (HiFunAdd, HiValueBytes ba, HiValueBytes bb) -> return $ HiValueBytes $ ba <> bb
      (HiFunAdd, HiValueList la, HiValueList lb) -> return $ HiValueList $ la <> lb
      (HiFunAdd, HiValueNumber na, HiValueNumber nb) -> return $ HiValueNumber $ na + nb
      (HiFunAdd, HiValueString sa, HiValueString sb) -> return $ HiValueString $ sa <> sb
      (HiFunAdd, HiValueTime ta, HiValueNumber na) -> return $ HiValueTime $ addUTCTime (fromRational na) ta
      (HiFunDiv, HiValueNumber na, HiValueNumber nb) -> case nb of
        0 -> throwE HiErrorDivideByZero
        _ -> return $ HiValueNumber $ na / nb
      (HiFunDiv, HiValueString sa, HiValueString sb) -> return $ HiValueString $ sa <> T.pack "/" <> sb
      (HiFunMul, HiValueNumber na, HiValueNumber nb) -> return $ HiValueNumber $ na * nb
      (HiFunMul, HiValueList la, HiValueNumber nb) -> return $ HiValueList $ stimes (fromEnum nb) la
      (HiFunMul, HiValueString sa, HiValueNumber nb) -> return $ HiValueString $ stimes (fromEnum nb) sa
      (HiFunMul, HiValueBytes ba, HiValueNumber nb) -> return $ HiValueBytes $ stimes (fromEnum nb) ba
      (HiFunSub, HiValueNumber na, HiValueNumber nb) -> return $ HiValueNumber $ na - nb
      (HiFunSub, HiValueTime ta, HiValueTime tb) -> return $ HiValueNumber $ toRational $ diffUTCTime ta tb
      (HiFunEquals, evaledA, evaledB) -> return $ HiValueBool $ evaledA == evaledB
      (HiFunGreaterThan, evaledA, evaledB) -> return $ HiValueBool $ evaledA > evaledB
      (HiFunLessThan, evaledA, evaledB) -> return $ HiValueBool $ evaledA < evaledB
      (HiFunNotEquals, evaledA, evaledB) -> return $ HiValueBool $ evaledA /= evaledB
      (HiFunNotGreaterThan, evaledA, evaledB) -> return $ HiValueBool $ evaledA <= evaledB
      (HiFunNotLessThan, evaledA, evaledB) -> return $ HiValueBool $ evaledA >= evaledB
      (HiFunRange, HiValueNumber na, HiValueNumber nb) -> return $ HiValueList $ fromList (Prelude.map HiValueNumber [na .. nb])
      (HiFunFold, HiValueFunction f, HiValueList lb) -> case lb of
        Seq.Empty -> return HiValueNull
        headL Seq.:<| tailL -> ExceptT $ foldM (evalFoldl f) (Right headL) tailL
      (HiFunAnd, HiValueBool ba, HiValueBool bb) -> return $ HiValueBool $ ba && bb
      (HiFunOr, HiValueBool ba, HiValueBool bb) -> return $ HiValueBool $ ba || bb
      (HiFunWrite, HiValueString doc, HiValueString text) ->
        return $ HiValueAction $ HiActionWrite (T.unpack doc) (toStrict $ encodeUtf8 $ Data.Text.Lazy.fromStrict text)
      (HiFunRand, HiValueNumber na, HiValueNumber nb) -> return $ HiValueAction $ HiActionRand (fromEnum na) (fromEnum nb)
      _ -> throwE $ if isBiFunc fun then HiErrorInvalidArgument else HiErrorArityMismatch
biFuncMap (HiValueString str) exprA exprB =
  getSliceTemplate exprA exprB (T.length str) (\l r -> HiValueString $ T.drop l $ T.take r str)
biFuncMap (HiValueList list) exprA exprB =
  getSliceTemplate exprA exprB (Seq.length list) (\l r -> HiValueList $ Seq.drop l $ Seq.take r list)
biFuncMap (HiValueBytes bytes) exprA exprB =
  getSliceTemplate
    exprA
    exprB
    (Data.ByteString.length bytes)
    (\l r -> HiValueBytes $ Data.ByteString.drop l $ Data.ByteString.take r bytes)
biFuncMap _ _ _ = return $ Left HiErrorInvalidFunction

getSliceTemplate :: HiMonad m => HiExpr -> HiExpr -> Int -> (Int -> Int -> HiValue) -> m (Either HiError HiValue)
getSliceTemplate exprA exprB len transformer = evalBiFuncTemplate exprA exprB $ \valA valB -> runExceptT $ do
  rrA <- ExceptT $ realNum len valA True
  rrB <- ExceptT $ realNum len valB False
  return $ transformer rrA rrB

isExpr :: HiExpr -> Bool
isExpr (HiExprApply _ _) = True
isExpr _ = False

evalFoldl :: HiMonad m => HiFun -> Either HiError HiValue -> HiValue -> m (Either HiError HiValue)
evalFoldl evaledFunc acc b =
  case acc of
    Left err -> return $ Left err
    Right valA -> biFuncMap (HiValueFunction evaledFunc) (HiExprValue valA) (HiExprValue b)

eval :: (HiMonad m) => HiExpr -> m (Either HiError HiValue)
eval (HiExprDict dict) = runExceptT $ do
  l <-
    foldM
      ( \list (a, b) -> do
          evaledA <- ExceptT $ eval a
          evaledB <- ExceptT $ eval b
          return $ (evaledA, evaledB) : list
      )
      []
      dict
  return $ HiValueDict $ Data.Map.fromList l
eval (HiExprRun expr) = runExceptT $ do
  ev <- ExceptT $ eval expr
  case ev of
    (HiValueAction evaledFunc) -> lift $ runAction evaledFunc
    _ -> throwE HiErrorInvalidFunction
eval (HiExprValue arg) = return $ Right arg
eval (HiExprApply fun args) = runExceptT $
  do
    f <- ExceptT $ eval fun
    case f of
      (HiValueFunction HiFunList) ->
        let bool = Prelude.any isExpr args
         in if bool
              then throwE HiErrorInvalidArgument
              else return $ HiValueList $ fromList $ Prelude.map (\case HiExprValue val -> val; _ -> HiValueNull) args
      rf ->
        case args of
          [a] -> ExceptT $ unFuncMap rf a
          [a, b] -> ExceptT $ biFuncMap rf a b
          [a, b, c] -> do
            case f of
              (HiValueFunction HiFunIf) -> do
                evaledA <- ExceptT $ eval a
                case evaledA of
                  (HiValueBool bool) ->
                    if bool then ExceptT $ eval b else ExceptT $ eval c
                  _ -> throwE HiErrorInvalidArgument
              _ -> throwE HiErrorArityMismatch
          _ -> throwE HiErrorArityMismatch

realNum :: HiMonad m => Int -> HiValue -> Bool -> m (Either HiError Int)
realNum len val bool = runExceptT $
  case val of
    HiValueNumber num
      | num < 0 -> return (fromEnum num + len)
      | otherwise -> return (fromEnum num)
    HiValueNull
      | bool -> return 0
      | otherwise -> return len
    _ -> throwE HiErrorInvalidArgument