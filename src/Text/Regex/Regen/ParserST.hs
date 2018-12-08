{-# LANGUAGE RecordWildCards #-}
module Text.Regex.Regen.ParserST where

import Control.Applicative (Alternative(..))
import Control.Monad (guard, when)
import Control.Monad.Catch (MonadThrow(..))
import Data.Attoparsec.Types ()
import Data.Char (chr, ord)
import Data.Functor (($>))
import Numeric (showHex, showOct)
import qualified Control.Exception as X
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator as P
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map

import Text.Regex.Regen.Pattern
import Text.Regex.Regen.PatternException

newtype ParserST a = ParserST
    { runParserST :: Pattern -> P.Parser (Pattern, a) }
instance Functor ParserST where
    fmap f p = ParserST $ \s -> do
        (s', a) <- runParserST p s
        pure (s', f a)
instance Applicative ParserST where
    pure  a = ParserST $ \s -> pure (s, a)
    f <*> m = ParserST $ \s -> do
        (s',  g) <- runParserST f s
        (s'', a) <- runParserST m s'
        pure (s'', g a)
instance Alternative ParserST where
    empty   = ParserST $ \s -> mempty
    p <|> q = ParserST $ \s -> runParserST p s <|> runParserST q s
instance Monoid (ParserST a) where
    mempty  = lift mempty
    mappend = (<|>)
instance Monad ParserST where
    m >>= f = ParserST $ \s -> do
        (s',  a) <- runParserST m s
        (s'', b) <- runParserST (f a) s'
        pure (s'', b)
instance MonadThrow ParserST where
    throwM e = ParserST $ \_ -> X.throw e

getPattern :: ParserST Pattern
getPattern = ParserST $ \s -> pure (s,s)

putPattern :: Pattern -> ParserST ()
putPattern s = ParserST $ \_ -> pure (s,())

modifyPattern :: (Pattern -> (Pattern, a)) -> ParserST a
modifyPattern f = ParserST $ \s -> pure (f s)

modifyPattern' :: (Pattern -> Pattern) -> ParserST ()
modifyPattern' f = modifyPattern $ \s -> (f s, ())

modifyOptions :: (Options -> (Options, a)) -> ParserST a
modifyOptions f = modifyPattern $ \s -> let (o,a) = f (pOptions s)
    in (s { pOptions = o }, a)

modifyOptions' :: (Options -> Options) -> ParserST ()
modifyOptions' f = modifyOptions $ \o -> (f o, ())

setIgnoreCase, setMultiline, setDupNames, setDotAll, setFreeSpacing :: Bool -> ParserST ()
setIgnoreCase  b = modifyOptions' $ \o -> o { oIgnoreCase  = b }
setMultiline   b = modifyOptions' $ \o -> o { oMultiline   = b }
setDupNames    b = modifyOptions' $ \o -> o { oDupNames    = b }
setDotAll      b = modifyOptions' $ \o -> o { oDotAll      = b }
setFreeSpacing b = modifyOptions' $ \o -> o { oFreeSpacing = b }

setStrategy :: Strategy -> ParserST ()
setStrategy s = modifyOptions' $ \o -> o { oDefaultStrategy = s }

setLineEnds :: LineEnds -> ParserST ()
setLineEnds n = modifyOptions' $ \o -> o { oLineEnds = n }

nextGroupNum :: ParserST Int
nextGroupNum = modifyPattern $
    \p -> let n = pGroupCount p + 1 in (p { pGroupCount = n }, n)

addGroup :: Int -> Part -> ParserST ()
addGroup n part = modifyPattern' $
    \p -> p { pGroups = Map.insert n (Group n part) (pGroups p) }

groupNumInUse :: Int -> ParserST Bool
groupNumInUse n = do
    Pattern {..} <- getPattern
    pure $ 0 < n && n <= pGroupCount

groupNameInUse :: S.ByteString -> ParserST Bool
groupNameInUse bs = do
    Pattern {..} <- getPattern
    pure $ bs `elem` Map.keys pGroupNames

lookupGroupIndex :: S.ByteString -> ParserST (Maybe Int)
lookupGroupIndex bs = do
    Pattern {..} <- getPattern
    pure $ Map.lookup bs pGroupNames

addNamedGroup :: S.ByteString -> Int -> Part -> ParserST ()
addNamedGroup bs n part = do
    inUse <- groupNameInUse bs
    when inUse $ eDupGroupName bs
    addGroup n part
    modifyPattern' $
        \p -> p { pGroupNames = Map.insert bs n (pGroupNames p) }

lift :: P.Parser a -> ParserST a
lift p = ParserST $ \s -> (,) s <$> p

char :: Char -> ParserST Char
char = lift . P.char

string :: S.ByteString -> ParserST S.ByteString
string = lift . P.string

satisfy :: (Char -> Bool) -> ParserST Char
satisfy = lift . P.satisfy

endOfInput :: ParserST ()
endOfInput = lift P.endOfInput

cInRange :: Char -> Char -> ParserST Char
cInRange l u = satisfy $ \c -> l <= c && c <= u

decimal :: Integral a => ParserST a
decimal = lift P.decimal

lookAhead :: ParserST a -> ParserST a
lookAhead p = ParserST $ \s -> P.lookAhead (runParserST p s)

choice :: [ParserST a] -> ParserST a
choice ps = ParserST $ \s -> P.choice (ps' s)
    where
    ps' s = (\p -> runParserST p s) <$> ps

infix 0 <?> 
(<?>) :: ParserST a -> String -> ParserST a
p <?> t = ParserST $ \s -> runParserST p s P.<?> t

peekChar :: ParserST (Maybe Char)
peekChar = lift P.peekChar

notFollowedBy :: ParserST a -> Char -> ParserST a
p `notFollowedBy` c = do
    x <- p
    m <- peekChar
    guard $ m /= Just c
    return x

peek :: ParserST a -> ParserST (Maybe a)
peek p = (Just <$> lookAhead p) <|> return Nothing

till :: ParserST a -> S.ByteString -> ParserST [a]
till p s = reverse <$> go []
    where
    go acc = do
        mb <- peek $ string s
        case mb of
            Just t | t == s -> string s $> acc
            _               -> p >>= \a -> go $ a:acc

takeWhile :: (Char -> Bool) -> ParserST S.ByteString
takeWhile p = lift $ P.takeWhile p

takeTill :: (Char -> Bool) -> ParserST S.ByteString
takeTill p = lift $ P.takeTill p

paren, bracketed, braced, angled, squoted, escaped :: ParserST a -> ParserST a
paren     p = char '('  *> p <* char ')'
braced    p = char '{'  *> p <* char '}'
bracketed p = char '['  *> p <* char ']'
angled    p = char '<'  *> p <* char '>'
squoted   p = char '\'' *> p <* char '\''
escaped   p = char '\\' *> p

digit, letter :: ParserST Char
digit  = lift P.digit
letter = lift P.letter_ascii

hexByte :: ParserST Char
hexByte = inRange $ char 'x' *> (byte' <|> braced byte' <|> pure '\0')
    where
    byte'      = byte2 <|> byte1
    byte2      = decode <$> hexDigit <*> hexDigit
    byte1      = decode <$> pure 0   <*> hexDigit
    decode a b = chr $ (a * 0x10) + b
    hexDigit   = hexDigit09 <|> hexDigitaz <|> hexDigitAZ
    hexDigit09 = (\c -> ord c - ord '0' + 0x0) <$> cInRange '0' '9'
    hexDigitaz = (\c -> ord c - ord 'a' + 0xa) <$> cInRange 'a' 'z'
    hexDigitAZ = (\c -> ord c - ord 'A' + 0xA) <$> cInRange 'A' 'Z'

octalByte :: ParserST Char
octalByte = inRange $ byte' <|> char 'o' *> (braced byte' <|> eMissingBrace)
    where
    octDigit      = (\c -> ord c - ord '0') <$> cInRange '0' '7'
    byte'         = byte3 <|> byte2 <|> byte1
    byte3         = decode <$> octDigit <*> octDigit <*> octDigit
    byte2         = decode <$> pure 0   <*> octDigit <*> octDigit
    byte1         = decode <$> pure 0   <*> pure 0   <*> octDigit
    decode a b c  = chr $ (a * 64) + (b * 8) + c
    eMissingBrace = eParserError "missing '{' after '\\o'"

inRange :: ParserST Char -> ParserST Char
inRange p = p >>= \c ->
    if '\x00' <= c && c <= '\x7f'
        then pure c
        else eParserError
            $ showString "octal and hex escapes must be in range "
            $ showRange '\x00' '\x7f' ""
    where
    showHex'  c   = showString "0x" . zeroPad 2 (showHex (ord c) "")
    showOct'  c   = zeroPad 3 $ showOct (ord c) ""
    zeroPad   n s = showString (replicate (n - length s) '0') . showString s
    showHexR  c d = showHex' c . showString "-" . showHex' d
    showOctR  c d = showOct' c . showString "-" . showOct' d
    showRange c d = showHexR c d . showString " (" . showOctR c d . showString ")"

cescape :: ParserST Char
cescape = choice
    [ '\t'   <$ char 't'
    , '\x1b' <$ char 'e'
    , '\n'   <$ char 'n'
    , '\f'   <$ char 'f'
    , '\r'   <$ char 'r'
    , '\a'   <$ char 'a'
    , '\v'   <$ char 'v'
    , '\\'   <$ char '\\'
    , char 'c' *> eNotSupported "'\\cx' escapes"
    ]

parseST :: ParserST a -> S.ByteString -> Either String (Pattern, a)
parseST = parseSTOpts defaultOptions

parseSTOpts :: Options -> ParserST a -> S.ByteString
            -> Either String (Pattern, a)
parseSTOpts o p = P.parseOnly $ runParserST p (emptyPattern o)
