module HW5.Parser
  ( HW5.Parser.parse,
  )
where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR, Postfix), makeExprParser)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Void (Void)
import HW5.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    many,
    manyTill,
    parse,
    satisfy,
    sepBy,
    sepBy1,
  )
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer (scientific, signed)
import qualified Text.Megaparsec.Char.Lexer as L

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = Text.Megaparsec.parse (parseExpr1 <* eof) ""

type Parser = Parsec Void String

parseNum :: Parser HiValue
parseNum = HiValueNumber . toRational <$> signed skipWs scientific

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

byteStringLiteral :: Parser String
byteStringLiteral = string "[#" *> manyTill L.charLiteral (string "#]")

parseString :: Parser HiValue
parseString = do
  HiValueString . T.pack <$> stringLiteral

parseByteString :: Parser HiValue
parseByteString = do
  HiValueBytes . (BS.pack . map (\n -> read ("0x" ++ n)) . words) <$> byteStringLiteral

parseBool :: Parser HiValue
parseBool =
  choice
    [ HiValueBool True <$ string "true",
      HiValueBool False <$ string "false"
    ]

parseList :: Parser [HiExpr]
parseList = between (skipWs *> char '[') (char ']') parseListArgs <* skipWs

parseListArgs :: Parser [HiExpr]
parseListArgs = skipWs *> parseExpr1 `sepBy` (skipWs *> char ',' <* skipWs)

parseMap :: Parser HiExpr
parseMap =
  HiExprDict <$> between (skipWs *> char '{') (char '}') parseMapArgs <* skipWs

parseMapArgs :: Parser [(HiExpr, HiExpr)]
parseMapArgs = skipWs *> parseMapPair `sepBy` (skipWs *> char ',' <* skipWs)

parseMapPair :: Parser (HiExpr, HiExpr)
parseMapPair = do
  key <- parseExpr1 <* skipWs <* char ':' <* skipWs
  value <- parseExpr1
  return (key, value)

parseFun :: Parser HiFun
parseFun =
  choice
    [ HiFunAdd <$ string "add",
      HiFunDiv <$ string "div",
      HiFunMul <$ string "mul",
      HiFunSub <$ string "sub",
      HiFunNotLessThan <$ string "not-less-than",
      HiFunNotGreaterThan <$ string "not-greater-than",
      HiFunNotEquals <$ string "not-equals",
      HiFunNot <$ string "not",
      HiFunAnd <$ string "and",
      HiFunOr <$ string "or",
      HiFunLessThan <$ string "less-than",
      HiFunGreaterThan <$ string "greater-than",
      HiFunEquals <$ string "equals",
      HiFunIf <$ string "if",
      HiFunLength <$ string "length",
      HiFunToUpper <$ string "to-upper",
      HiFunToLower <$ string "to-lower",
      HiFunReverse <$ string "reverse",
      HiFunTrim <$ string "trim",
      HiFunList <$ string "list",
      HiFunRange <$ string "range",
      HiFunFold <$ string "fold",
      HiFunPackBytes <$ string "pack-bytes",
      HiFunUnpackBytes <$ string "unpack-bytes",
      HiFunEncodeUtf8 <$ string "encode-utf8",
      HiFunDecodeUtf8 <$ string "decode-utf8",
      HiFunZip <$ string "zip",
      HiFunUnzip <$ string "unzip",
      HiFunSerialise <$ string "serialise",
      HiFunDeserialise <$ string "deserialise",
      HiFunRead <$ string "read",
      HiFunWrite <$ string "write",
      HiFunMkDir <$ string "mkdir",
      HiFunChDir <$ string "cd",
      HiFunParseTime <$ string "parse-time",
      HiFunRand <$ string "rand",
      HiFunEcho <$ string "echo",
      HiFunCount <$ string "count",
      HiFunKeys <$ string "keys",
      HiFunValues <$ string "values",
      HiFunInvert <$ string "invert"
    ]

symbol :: String -> Parser String
symbol = L.symbol skipWs

binaryLeft :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryLeft name f = InfixL (name >> return f)

binaryRight :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryRight name f = InfixR (name >> return f)

binaryNone :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryNone name f = InfixN (name >> return f)

parseExpr1 :: Parser HiExpr
parseExpr1 = makeExprParser parseExpr operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ Postfix (symbol "!" >> return HiExprRun)
    ],
    [ binaryLeft (symbol "*") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunMul) [a, b],
      binaryLeft (try $ symbol "/" <* notFollowedBy (symbol "=")) $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunDiv) [a, b]
    ],
    [ binaryLeft (symbol "-") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunSub) [a, b],
      binaryLeft (symbol "+") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunAdd) [a, b]
    ],
    [ binaryNone (symbol "==") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunEquals) [a, b],
      binaryNone (symbol "/=") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunNotEquals) [a, b],
      binaryNone (symbol ">=") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunNotLessThan) [a, b],
      binaryNone (symbol "<=") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunNotGreaterThan) [a, b],
      binaryNone (symbol ">") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunGreaterThan) [a, b],
      binaryNone (symbol "<") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunLessThan) [a, b]
    ],
    [ binaryRight (symbol "&&") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunAnd) [a, b]
    ],
    [ binaryRight (symbol "||") $ \a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunOr) [a, b]
    ]
  ]

parseOneWord :: String -> HiValue -> Parser HiValue
parseOneWord keyword action = do
  _ <- string keyword
  return action

parseCwd :: Parser HiValue
parseCwd = parseOneWord "cwd" (HiValueAction HiActionCwd)

parseNow :: Parser HiValue
parseNow = parseOneWord "now" (HiValueAction HiActionNow)

parseNull :: Parser HiValue
parseNull = parseOneWord "null" HiValueNull

parseExpr :: Parser HiExpr
parseExpr = do
  skipWs
  parsed <-
    choice
      [ char '(' *> skipWs *> parseExpr1 <* skipWs <* char ')' <* skipWs >>= parseExprOrArgs,
        parseFun >>= \fun -> parseExprOrArgs $ HiExprValue $ HiValueFunction fun,
        parseNum >>= \num -> parseExprOrArgs $ HiExprValue num,
        parseBool >>= \bool -> parseExprOrArgs $ HiExprValue bool,
        parseString >>= \str -> parseExprOrArgs $ HiExprValue str,
        parseNull >>= \nil -> parseExprOrArgs $ HiExprValue nil,
        parseByteString >>= \bytes -> parseExprOrArgs $ HiExprValue bytes,
        parseList >>= \args -> parseExprOrArgs $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) args,
        parseCwd >>= \cwd -> parseExprOrArgs $ HiExprValue cwd,
        parseNow >>= \now -> parseExprOrArgs $ HiExprValue now,
        parseMap >>= \m -> parseExprOrArgs m
      ]
  skipWs
  return parsed

parseExprOrArgs :: HiExpr -> Parser HiExpr
parseExprOrArgs expr = do
  skipWs
  choice
    [ do
        checkChar '('
        skipWs
        args <- parseArgs
        skipWs
        checkChar ')'
        skipWs
        parseExprOrArgs $ HiExprApply expr args,
      do
        checkChar '.'
        skipWs
        str <- intercalate "-" <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
        skipWs
        parseExprOrArgs $ HiExprApply expr [HiExprValue $ HiValueString $ T.pack str],
      return
        expr
    ]

parseArgs :: Parser [HiExpr]
parseArgs =
  do
    skipWs
    choice
      [ do
          expr <- parseExpr1
          skipWs
          args1 <- parseSubArgs
          skipWs
          return $ expr : args1,
        return []
      ]

parseSubArgs :: Parser [HiExpr]
parseSubArgs =
  do
    skipWs
    choice
      [ do
          skipWs
          checkChar ','
          skipWs
          expr <- parseExpr1
          skipWs
          args <- parseSubArgs
          skipWs
          return $ expr : args,
        return []
      ]

checkChar :: Char -> Parser ()
checkChar ch = void $ char ch

skipWs :: Parser ()
skipWs =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")
