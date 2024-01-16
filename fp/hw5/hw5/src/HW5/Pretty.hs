{-# OPTIONS_GHC -Wno-unused-matches #-}

module HW5.Pretty
  ( prettyValue,
  )
where

import qualified Data.ByteString
import Data.Foldable
import qualified Data.Map
import Data.Ratio
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import qualified Data.Sequence as Seq
import HW5.Base (HiAction (..), HiFun (..), HiValue (..))
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction biFunction) = annotate (color Blue) $
  case biFunction of
    HiFunAdd -> pretty "add"
    HiFunSub -> pretty "sub"
    HiFunMul -> pretty "mul"
    HiFunDiv -> pretty "div"
    HiFunNot -> pretty "not"
    HiFunAnd -> pretty "and"
    HiFunOr -> pretty "or"
    HiFunLessThan -> pretty "less-than"
    HiFunGreaterThan -> pretty "greater-than"
    HiFunEquals -> pretty "equals"
    HiFunNotLessThan -> pretty "not-less-than"
    HiFunNotGreaterThan -> pretty "not-greater-than"
    HiFunNotEquals -> pretty "not-equals"
    HiFunIf -> pretty "if"
    HiFunLength -> pretty "length"
    HiFunReverse -> pretty "reverse"
    HiFunToUpper -> pretty "to-upper"
    HiFunToLower -> pretty "to-lower"
    HiFunTrim -> pretty "trim"
    HiFunFold -> pretty "fold"
    HiFunList -> pretty "list"
    HiFunRange -> pretty "range"
    HiFunPackBytes -> pretty "pack-bytes"
    HiFunUnpackBytes -> pretty "unpack-bytes"
    HiFunZip -> pretty "zip"
    HiFunUnzip -> pretty "unzip"
    HiFunEncodeUtf8 -> pretty "encode-utf8"
    HiFunDecodeUtf8 -> pretty "decode-utf8"
    HiFunSerialise -> pretty "serialise"
    HiFunDeserialise -> pretty "deserialise"
    HiFunRead -> pretty "read"
    HiFunWrite -> pretty "write"
    HiFunMkDir -> pretty "mkdir"
    HiFunChDir -> pretty "cd"
    HiFunParseTime -> pretty "parse-time"
    HiFunRand -> pretty "rand"
    HiFunEcho -> pretty "echo"
    HiFunCount -> pretty "count"
    HiFunInvert -> pretty "invert"
    HiFunKeys -> pretty "keys"
    HiFunValues -> pretty "values"
prettyValue (HiValueList listVal) = annotate (color Green) $
  case listVal of
    Seq.Empty -> pretty "[]"
    headL Seq.:<| tailL ->
      pretty "[ " <> hcat (punctuate (pretty ", ") (map prettyValue (toList listVal))) <> pretty " ]"
prettyValue (HiValueNumber i) =
  annotate (color White) $
    let num = numerator i
        denom = denominator i
        (whole, remainder) = quotRem num denom
     in case denom of
          1 -> pretty num
          _ -> case fromRationalRepetendUnlimited i of
            (a, Nothing) -> pretty (formatScientific Fixed Nothing a)
            _ ->
              case whole of
                0 -> pretty (show num ++ "/" ++ show denom)
                _ -> pretty (show whole ++ (if num > 0 then " + " else " - ") ++ show (abs remainder) ++ "/" ++ show denom)
prettyValue (HiValueBool i) =
  annotate (color Yellow) $ if i then pretty "true" else pretty "false"
prettyValue HiValueNull = annotate (color Red) $ pretty "null"
prettyValue (HiValueString text) = annotate (color Magenta) $ viaShow text
prettyValue (HiValueBytes bytes) =
  annotate (color Green) $
    pretty "[# "
      <> pretty
        (unwords $ map (\n -> let s = showHex n "" in if length s == 1 then "0" <> s else s) (Data.ByteString.unpack bytes))
      <> pretty " #]"
prettyValue (HiValueTime time) = annotate (color Cyan) $ viaShow time
prettyValue (HiValueAction action) = annotate (color Blue) $
  case action of
    (HiActionChDir dir) -> pretty "cd(" <> viaShow dir <> pretty ")"
    HiActionCwd -> pretty "cwd"
    HiActionMkDir dir -> pretty "mkdir(" <> viaShow dir <> pretty ")"
    HiActionRead dir -> pretty "read(" <> viaShow dir <> pretty ")"
    HiActionWrite file text -> pretty "write(" <> viaShow file <> pretty ", " <> viaShow text <> pretty ")"
    HiActionNow -> pretty "now"
    (HiActionRand l r) -> pretty "rand(" <> viaShow l <> pretty ", " <> viaShow r <> pretty ")"
    (HiActionEcho t) -> pretty t
prettyValue (HiValueDict dict) =
  annotate (color Green) $
    pretty "{ "
      <> hcat (punctuate (pretty ", ") $ map (\(k, v) -> prettyValue k <> pretty ": " <> prettyValue v) (Data.Map.toList dict))
      <> pretty " }"