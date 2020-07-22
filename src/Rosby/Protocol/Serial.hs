module Rosby.Protocol.Parser where

import Control.Applicative (Alternative((<|>)))
import Data.ByteString (ByteString())
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Attoparsec.ByteString (Parser(), (<?>))
import qualified Data.Attoparsec.ByteString as P
import Data.Word8

data Prim
  = Str Int ByteString -- ^ Strings are indexed by their length
  | Num Integer -- ^ Numbers are integers
  | Array Int [Prim] -- ^ Arrays are indexed by their length and can
                     -- contain Str and Int types
  deriving (Eq, Show)

isCR = (== 13)
isLF = (== 10)

lineSep :: Parser ()
lineSep = P.skip isCR >> P.skip isLF <?> "lineSep"

strParser :: Parser Prim
strParser = do
  P.word8 _dollar
  num <- P.takeWhile isDigit
  lineSep
  case B8.readInt num of
    Just (num', _) -> do
      str <- P.take num'
      lineSep
      pure $ Str num' str
    Nothing        -> fail "String length must be an integer"
  where
    isDigit c = c >= 48 && c <= 57

intParser :: Parser Prim
intParser = do
  P.word8 _colon
  num <- P.takeWhile isDigit
  lineSep
  case B8.readInteger num of
    Just (num', _) -> pure $ Num num'
    Nothing        -> fail "Failed to parse integer"

arrayParser :: Parser Prim
arrayParser = do
  P.word8 _asterisk
  num <- P.takeWhile isDigit
  lineSep
  case B8.readInt num of
    Just (num', _) -> do
      prims <- P.count num' (strParser <|> intParser)
      pure $ Array num' prims
    Nothing -> fail "Array length must be integer"

primParser :: Parser Prim
primParser = arrayParser <|> strParser <|> intParser
