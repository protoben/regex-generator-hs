{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Regex.Regen.Parse where

import Prelude hiding (takeWhile)

import Control.Applicative (Alternative(..), optional)
import Control.Monad (when, unless, guard)
import Control.Monad.Catch (MonadThrow)
import Data.Char (toLower, toUpper, isAlpha)
import Data.Functor (($>))
import Data.List ((\\), nub)
import Data.Map (Map)
import Data.Set (Set)
import qualified Control.Exception as X
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator as P
import qualified Data.ByteString.Char8 as S
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Regex.Regen.ParserST
import Text.Regex.Regen.Pattern
import Text.Regex.Regen.PatternException
import Text.Regex.Regen.Util
import Text.Regex.Regen.CClass (bracketClass, backslashClass, posixRange)

groupName :: Char -> ParserST S.ByteString
groupName term = check =<< takeTill (==term)
    where
    isNameC = (=='_') ||| Char.isAlphaNum
    check n
        | Char.isDigit (S.head n) = eBadGroupName "must not start with a digit"
        | not (S.all isNameC n)   = eBadGroupName "invalid character"
        | S.length n > 32         = eBadGroupName "longer than 32 characters"
        | otherwise               = pure n

group :: ParserST Part
group = paren $ nonCap <|> atomic <|> reset <|> capture
    where
    nonCap  = string "?:" *> parts
    atomic  = string "?>" *> parts
    reset   = string "?|" *> eNotSupported "reset groups"
    gName   = optional (char 'P') *> angled (groupName '>') <|> squoted (groupName '\'')
    capture = do
        num  <- nextGroupNum
        name <- Just <$> (char '?' *> gName) <|> pure Nothing
        part <- parts
        case name of
            Just bs -> addNamedGroup bs num part
            Nothing -> addGroup num part
        pure $ Reference num

byte :: ParserST Part
byte = do
    Options {..} <- pOptions <$> getPattern
    let isMeta  = isMeta' oFreeSpacing
        meta    = satisfy isMeta
        literal = satisfy $ not . isMeta
        escLit  = satisfy $ not . (=='\\') &&& not . isSpecial
    c <- choice
        [ escaped cescape
        , escaped hexByte
        , escaped octalByte
        , escaped meta
        , escaped escLit
        , literal
        ]
    pure $ doCases oIgnoreCase c
    where
    isMeta' b c = c `S.elem` ("\\^$.[|()?*+" `S.append` if b then " " else "")
    isSpecial c = c `S.elem` "ABCDEGHKLNPQRSUVWXZabcdefghklnoprstuvwxz1234567"
    doCases True c | isAlpha c = CClass True [toLower c, toUpper c]
    doCases _    c             = Byte c

forbidden :: ParserST Part
forbidden = choice
    [ string "\\C" *> eNotSupported "'\\C'"
    , string "\\X" *> eNotSupported "'\\X'"
    , string "\\L" *> eNotSupported "'\\L'"
    , string "\\l" *> eNotSupported "'\\l'"
    , string "\\U" *> eNotSupported "'\\U'"
    , string "\\u" *> eNotSupported "'\\u'"
    , string "\\Q" *> eNotSupported "'\\Q'"
    , string "\\E" *> eNotSupported "'\\E'"
    , posixRange   *> eNotSupported "posix range outside of class"
    , range        *> eParserError "nothing to quantify"
    ]
    where
    eBadRef c = eNoSuchIndex $ Left (Char.ord c - Char.ord '0')

cclass :: ParserST Part
cclass = doCases =<< (bracketClass <|> backslashClass)
    where
    doCases (CClass b cs) = do
        ignoreCase <- oIgnoreCase . pOptions <$> getPattern
        pure . CClass b . nub $ if ignoreCase
            then foldr (\c -> ([toLower c, toUpper c] ++)) [] cs
            else cs

range :: ParserST Range
range = plus <|> star <|> question <|> braced range'
    where
    plus     = Range 1 Nothing  <$ char '+'
    star     = Range 0 Nothing  <$ char '*'
    question = Range 0 (Just 1) <$ char '?'
    range'   = check =<< (bounded <|> lower <|> fixed)
    bounded  = Range <$> decimal <* char ',' <*> fmap Just decimal
    lower    = Range <$> decimal <* char ',' <*> pure Nothing
    fixed    = decimal >>= \n -> pure $ Range n (Just n)
    check r@(Range n m)
        | maybe False (n >) m = eQuantOutOfOrder
        | otherwise           = pure r

quantified :: ParserST Part -> ParserST Part
quantified p = Quantified <$> p <*> range <*> strategy
    where
    strategy   = lazy <|> possessive <|> def
    lazy       = Lazy       <$ char '?' *> eNotSupported "lazy quantifiers"
    possessive = Possessive <$ char '+' *> eNotSupported "possessive quantifiers"
    def        = oDefaultStrategy . pOptions <$> getPattern

reference :: ParserST Part
reference = do
    m     <- pGroupCount <$> getPattern
    ix    <- numbered m <|> named
    inUse <- either groupNumInUse groupNameInUse ix
    when (not inUse) $ eNoSuchIndex ix
    Reference <$> case ix of
        Left  n  -> pure n
        Right bs -> lookupGroupIndex bs >>= maybe (eNoSuchName bs) pure
    where
    named = Right <$> choice
        [ string "\\k" *> angled  (groupName '>')
        , string "\\k" *> squoted (groupName '\'')
        , string "\\k" *> braced  (groupName '}')
        , string "\\g" *> braced  (groupName '}')
        , paren $ string "?P=" *> (groupName ')')
        ]
    numbered m = Left <$> choice
        [ string "\\"  *> decimal >>= \n -> guard (0 < n && n <= max 7 m) >> pure n
        , string "\\"  *> braced decimal
        , string "\\g" *> decimal
        , string "\\g" *> braced decimal
        , string "\\g" *> braced (char '-' *> decimal >>= \n -> pure $ m-n + 1)
        ]
    eNoSuchName = eNoSuchIndex . Right

call :: ParserST Part
call = do
    m  <- pGroupCount <$> getPattern
    ix <- numbered m <|> named
    pure $ Call ix
    where
    named = Right <$> choice
        [ paren $ string "?&"  *> groupName ')'
        , paren $ string "?P>" *> groupName ')'
        , escaped $ char 'g' *> angled  (groupName '>')
        , escaped $ char 'g' *> squoted (groupName '\'')
        ]
    numbered m = Left <$> choice
        [ paren $ string "?R" *> pure 0
        , paren $ char '?' *> decimal
        , paren $ char '?' *> relative m
        , escaped $ char 'g' *> angled decimal
        , escaped $ char 'g' *> angled (relative m)
        , escaped $ char 'g' *> squoted decimal
        , escaped $ char 'g' *> squoted (relative m)
        ]
    relative m = do
        op <- (-) <$ char '-' <|> (+) <$ char '+'
        n  <- decimal
        pure $ m `op` n + 1

anchors :: ParserST [Anchor]
anchors = do
    Options {..} <- pOptions <$> getPattern
    choice
        [ string "^"   $> if oMultiline then mlCaret  else caret
        , string "$"   $> if oMultiline then mlDollar else dollar
        , string "\\A" $> [Start]
        , string "\\Z" $> [End, EndBeforeNewline]
        , string "\\z" $> [End]
        , string "\\b" $> [WordBoundary, Start, End]
        , string "\\B" $> [WordInternal]
        , string "\\G" >>= \_ -> eNotSupported "start-of-match anchor"
        , string "\\K" >>= \_ -> eNotSupported "start-of-match reset"
        ]
    where
    caret    = [Start]
    mlCaret  = [Start, AfterNewline]
    dollar   = [End, EndBeforeNewline]
    mlDollar = [End, EndBeforeNewline, BeforeNewline]

conditional :: ParserST Part
conditional = string "(?" *> paren (choice
    [ decimal                      >> eNoConditionals
    , char '+'    *> decimal       >> eNoConditionals
    , char '-'    *> decimal       >> eNoConditionals
    , char 'R'    *> decimal       >> eNoConditionals
    , string "R&" *> groupName ')' >> eNoConditionals
    , char 'R'                     >> eNoConditionals
    , angled (groupName '>')       >> eNoConditionals
    , squoted (groupName '\'')     >> eNoConditionals
    , groupName ')'                >> eNoConditionals
    , string "DEFINE"              >> eNoConditionals
    , lookAround                   >> eNoConditionals
    , takeTill (==')')             >> eMalformed
    ])
    where
    eMalformed      = eParserError "malformed name or number after '(?('"
    eNoConditionals = eNotSupported "conditionals"

empty' :: ParserST Part
empty' = pure Empty

lookAround :: ParserST ()
lookAround = lookAround' *> eNotSupported "lookarounds" <?> "lookaround"
    where
    lookAround' = aheadPos <|> aheadNeg <|> behindPos <|> behindNeg
    aheadPos    = string "(?="  *> content <* char ')'
    aheadNeg    = string "(?!"  *> content <* char ')'
    behindPos   = string "(?<=" *> content <* char ')'
    behindNeg   = string "(?<!" *> content <* char ')'
    content     = many $ satisfy (/=')')

verb :: ParserST ()
verb = paren $ char '*' *> choice
    [ string "ACCEPT"          >>  eNoBacktrackVerbs
    , string "FAIL"            >>  eNoBacktrackVerbs
    , string "F"               >>  eNoBacktrackVerbs
    , string "MARK:"   *> name >>  eNoBacktrackVerbs
    , string ":"       *> name >>  eNoBacktrackVerbs
    , string "COMMIT:" *> name >>  eNoBacktrackVerbs
    , string "COMMIT"          >>  eNoBacktrackVerbs
    , string "PRUNE:"  *> name >>  eNoBacktrackVerbs
    , string "PRUNE"           >>  eNoBacktrackVerbs
    , string "SKIP:"   *> name >>  eNoBacktrackVerbs
    , string "SKIP"            >>  eNoBacktrackVerbs
    , takeTill (==')')         >>= eNoSuchVerb . show
    ]
    where
    eNoBacktrackVerbs = eNotSupported "backtracking verbs"
    name              = takeTill (/=')')

globalOption :: ParserST ()
globalOption = paren $ char '*' >> choice
    [ () <$ string "LIMIT_MATCH="     <* decimal
    , () <$ string "LIMIT_RECURSION=" <* decimal
    , () <$ string "NO_AUTO_POSSESS"
    , () <$ string "NO_START_OPT"
    , () <$ string "BSR_ANYCRLF"
    , string "BSR_UNICODE" >>  noUnicode
    , string "UTF8"        >>  noUnicode
    , string "UTF16"       >>  noUnicode
    , string "UTF32"       >>  noUnicode
    , string "UTF"         >>  noUnicode
    , string "UCP"         >>  noUnicode
    , string "CRLF"        >>  noCRLF
    , string "ANYCRLF"     >>  setLineEnds AnyCRLF
    , string "ANY"         >>  setLineEnds Any
    , string "CR"          >>  setLineEnds CR
    , string "LF"          >>  setLineEnds LF
    , takeTill (==')')     >>= eBadOpt
    ]
    where
    eBadOpt s = eParserError $ "bad global option: " ++ show s
    noUnicode = eNotSupported "unicode"
    noCRLF    = eNotSupported "crlf line ends"

localOption :: ParserST ()
localOption = paren $ do
    char '?'
    options
    checkOptions =<< fmap pOptions getPattern
    where
    options       = posOpts <* char '-' <* negOpts <|> posOpts
    posOpts       = () <$ many (option True)
    negOpts       = () <$ many (option False)
    setUngreedy b = setStrategy $ if b then Lazy else Greedy
    option      b = choice
        [ setIgnoreCase  b <* char 'i'
        , setMultiline   b <* char 'm'
        , setDupNames    b <* char 'J'
        , setDotAll      b <* char 's'
        , setUngreedy    b <* char 'U'
        , setFreeSpacing b <* char 'x'
        ]

comment :: ParserST ()
comment = do
    ignoreSpace <- oFreeSpacing . pOptions <$> getPattern
    () <$ (spaceIf ignoreSpace <|> comment')
    where
    spaceIf b =         guard b     *> fmap S.singleton (satisfy Char.isSpace)
    comment'  = paren $ string "?#" *> takeTill (==')')

callout :: ParserST ()
callout = paren $
    string "?C" *> takeTill (==')') *> eNotSupported "callouts"

parts :: ParserST Part
parts = parts' <* many comment
    where
    parts'     = alt anchored <|> anchored
    anchored   = sequence `anch` anchored <|> sequence
    sequence   = nonEmptySeq <$> many grouping
    grouping   = quantified atom' <|> atom'
    atom'      = silent *> atom' <|> atom
    atom       = reference <|> conditional <|> call <|> group <|> forbidden <|> cclass <|> byte
    silent     = localOption <|> verb <|> lookAround <|> callout <|> comment
    alt p      = Alternative .: (:) <$> p <*> some (char '|' *> p)
    p `anch` q = choice
        [ Anchored <$> p      <*> anchors <*> q
        , Anchored <$> empty' <*> anchors <*> q
        , Anchored <$> p      <*> anchors <*> empty'
        ]

parsePatternOpts :: Options -> S.ByteString -> IO Pattern
parsePatternOpts o bs = case parseSTOpts o parts' bs of
    Left  e            -> X.throwIO $ EParserError e
    Right (pattern,ps) -> do
        checkOptions o
        let groups   = pGroups pattern
            topLevel = Group 0 ps
            groups'  = Map.insert 0 topLevel groups
            pattern' = pattern {pGroups = groups'}
        pure pattern'
    where
    parts' = many globalOption *> parts <* endOfInput

parsePattern :: S.ByteString -> IO Pattern
parsePattern = parsePatternOpts defaultOptions

checkOptions :: MonadThrow m => Options -> m ()
checkOptions Options {..}
    | oRecursion                 = eNotSupported "recursion"
    | oDefaultStrategy /= Greedy = eNotSupported "lazy quantifiers"
    | oDupNames                  = eNotSupported "duplicate group names"
    | oLineEnds == Any           = eNotSupported "unicode"
    | oLineEnds == CRLF          = eNotSupported "crlf line ends"
    | otherwise                  = pure ()
