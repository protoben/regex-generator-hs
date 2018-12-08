{-# LANGUAGE OverloadedStrings #-}
module Text.Regex.Regen.CClass where

import Prelude hiding (takeWhile)

import Control.Applicative (Alternative(..))
import Data.Attoparsec.ByteString.Char8 (isDigit, isSpace, isHorizontalSpace)
import Data.Char (ord)
import Data.Functor (($>))
import Data.List ((\\))

import Text.Regex.Regen.ParserST
import Text.Regex.Regen.Pattern
import Text.Regex.Regen.PatternException
import Text.Regex.Regen.Util

bracketClass :: ParserST Part
bracketClass = bracketed $ negative <|> positive
    where
    positive = fmap (CClass True)  $             go []
    negative = fmap (CClass False) $ char '^' *> go []
    cpart    = posixRange <|> backslashRange <|> dashRange <|> singleton
    go acc   = do
        acc' <- (acc ++) <$> cpart
        mb   <- peekChar
        case mb of
            Just ']' -> return acc'
            _        -> go acc'

backslashClass :: ParserST Part
backslashClass = do
    o <- pOptions <$> getPattern
    dot o <|> bsR <|> bsN o <|> range
    where
    dot o = string "."   $> CClass False (munless (oDotAll o) (oLineEndChars o))
    bsN o = string "\\N" $> CClass False (oLineEndChars o)
    bsR   = string "\\R" $> CClass True ['\r','\n','\f','\v']
    range = CClass True <$> backslashRange

posixRange :: ParserST [Char]
posixRange = toPC =<< string "[:" *> pcname <* string ":]"
    where
    toPC ('^':s)  = negate <$> toPC s
    toPC "alnum"  = return $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    toPC "alpha"  = return $ ['a'..'z'] ++ ['A'..'Z']
    toPC "ascii"  = return $ ['\x00'..'\x7f']
    toPC "blank"  = return $ [' ','\t']
    toPC "cntrl"  = return $ '\x7f' : ['\x00'..'\x1f']
    toPC "digit"  = return $ ['0'..'9']
    toPC "graph"  = return $ ['\x21'..'\x7e']
    toPC "lower"  = return $ ['a'..'z']
    toPC "print"  = return $ ['\x20'..'\x7e']
    toPC "punct"  = return $ "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
    toPC "space"  = return $ " \t\r\n\v\f" 
    toPC "word"   = return $ '_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    toPC "xdigit" = return $ ['a'..'f'] ++ ['A'..'Z'] ++ ['0'..'9']
    toPC name     = eNoSuchClass name
    negate cs     = ['\x00'..'\xff'] \\ cs
    pcname        = many $ satisfy (/=':') <|> char ':' `notFollowedBy` ']'

backslashRange :: ParserST [Char]
backslashRange = let bytes = ['\x00'..'\xff'] in choice
    [ string "\\d" $> [ c | c <- bytes, isDigit c ]
    , string "\\D" $> [ c | c <- bytes, not (isDigit c) ]
    , string "\\h" $> [ c | c <- bytes, isHSpace c ]
    , string "\\H" $> [ c | c <- bytes, not (isHSpace c) ]
    , string "\\s" $> [ c | c <- bytes, isSpace c ]
    , string "\\S" $> [ c | c <- bytes, not (isSpace c) ]
    , string "\\v" $> "\n\v\f\r"
    , string "\\V" $> bytes \\ "\n\v\f\r"
    , string "\\w" $> '_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    , string "\\W" $> bytes \\ ('_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    , string "\\N" >> eNotSupported "'\\N' in character class"
    , string "\\L" >> eNotSupported "'\\L'"
    , string "\\l" >> eNotSupported "'\\l'"
    , string "\\U" >> eNotSupported "'\\U'"
    , string "\\u" >> eNotSupported "'\\u'"
    , string "\\Q" >> eNotSupported "'\\Q' in character class"
    , string "\\E" >> eNotSupported "'\\E' in character class"
    ,                   string "\\p{" *> some letter <* char '}' >> eNoProps
    ,                   string "\\p"  *> letter                  >> eNoProps
    , fmap (bytes \\) $ string "\\P{" *> some letter <* char '}' >> eNoProps
    , fmap (bytes \\) $ string "\\P"  *> letter                  >> eNoProps
    ]
    where
    eNoProps  = eNotSupported "properties"
    isHSpace  = isHorizontalSpace . fromIntegral . ord

dashRange :: ParserST [Char]
dashRange = mkRange =<< (,) <$> singleton <* char '-' <*> singleton
    where
    mkRange p = case p of
        ([a],[b]) | a <= b -> pure [a..b]
        ([_],[_])          -> eClassOutOfOrder
        _                  -> eParserError "unreachable"

singleton :: ParserST [Char]
singleton = (:[]) <$> choice
    [ escaped hexByte
    , escaped octalByte
    , escaped cescape
    , escaped literal
    , literal
    ]
    where
    literal = satisfy (/='\\')
