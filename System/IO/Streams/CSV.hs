module System.IO.Streams.CSV
    ( CSV
    , Row
    , toCSV
    , toCSVRow
    , csv
    , row
    ) where

import Control.Applicative
import Data.Attoparsec (Parser)
import qualified Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as AC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid (mempty)
import Data.Word (Word8)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams.Attoparsec as SA

type Row = [ByteString]
type CSV = [Row]

csv :: Parser [Row]
csv = do
    r <- row
    end <- P.atEnd
    if end
        then return (maybe [] (:[]) r)
        else do
            rest <- csv
            return (maybe rest (:rest) r)

row :: Parser (Maybe Row)
row = csvrow <|> badrow

badrow :: Parser (Maybe Row)
badrow = P.takeWhile (not . AC.isEndOfLine) *>
    (AC.endOfLine <|> AC.endOfInput) *> return Nothing

csvrow :: Parser (Maybe Row)
csvrow = properrow >>= return . Just
  where
    properrow = rowbody <* (AC.endOfLine <|> P.endOfInput)
    rowbody = (quotedField' <|> field) `P.sepBy` AC.char csvSep
    quotedField' = maybe mempty (P.try . quotedField) csvQuoteChar

field :: Parser ByteString
field = P.takeWhile isFieldChar

csvSep :: Char
csvSep = ','

quotedField :: Char -> Parser ByteString
quotedField quoteChar
    = AC.char quoteChar
    *> (BC.pack <$> many (AC.notChar quoteChar <|> quoted))
    <* AC.char quoteChar
  where
    dbl = BC.pack [quoteChar, quoteChar]
    quoted = P.string dbl *> return quoteChar

csvQuoteChar :: Maybe Char
csvQuoteChar = Nothing

isFieldChar :: Word8 -> Bool
isFieldChar = P.notInClass xs'
  where
    xs = csvSep:"\n\r"
    xs' = maybe xs (:xs) csvQuoteChar

toCSV :: InputStream ByteString -> IO CSV
toCSV = SA.parseFromStream csv

toCSVRow :: InputStream ByteString -> IO (InputStream Row)
toCSVRow = SA.parserToInputStream row
